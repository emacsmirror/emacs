;;; easy-mmode.el --- easy definition for major and minor modes  -*- lexical-binding: t; -*-

;; Copyright (C) 1997, 2000-2025 Free Software Foundation, Inc.

;; Author: Georges Brun-Cottan <Georges.Brun-Cottan@inria.fr>
;; Maintainer: Stefan Monnier <monnier@gnu.org>
;; Package: emacs

;; Keywords: extensions lisp

;; This file is part of GNU Emacs.

;; GNU Emacs is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; GNU Emacs is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs.  If not, see <https://www.gnu.org/licenses/>.

;;; Commentary:

;; Minor modes are useful and common.  This package makes defining a
;; minor mode easy, by focusing on the writing of the minor mode
;; functionalities themselves.  Moreover, this package enforces a
;; conventional naming of user interface primitives, making things
;; natural for the minor-mode end-users.

;; For each mode, easy-mmode defines the following:
;; <mode>      : The minor mode predicate.  A buffer-local variable.
;; <mode>-map  : The keymap possibly associated to <mode>.
;;       see `define-minor-mode' documentation
;;
;; eval
;;  (pp (macroexpand '(define-minor-mode <your-mode> <doc>)))
;; to check the result before using it.

;; The order in which minor modes are installed is important.  Keymap
;; lookup proceeds down minor-mode-map-alist, and the order there
;; tends to be the reverse of the order in which the modes were
;; installed.  Perhaps there should be a feature to let you specify
;; orderings.

;; Additionally to `define-minor-mode', the package provides convenient
;; ways to define keymaps, and other helper functions for major and minor modes.

;;; Code:

(defun easy-mmode-pretty-mode-name (mode &optional lighter)
  "Turn the symbol MODE into a string intended for the user.
If provided, LIGHTER will be used to help choose capitalization by,
replacing its case-insensitive matches with the literal string in LIGHTER."
  (let* ((case-fold-search t)
	 ;; Produce "Foo-Bar minor mode" from foo-bar-minor-mode.
	 (name (concat (replace-regexp-in-string
			;; If the original mode name included "-minor" (some
			;; of them don't, e.g. auto-revert-mode), then
			;; replace it with " minor".
			"-Minor" " minor"
			;; "foo-bar-minor" -> "Foo-Bar-Minor"
			(capitalize (replace-regexp-in-string
				     ;; "foo-bar-minor-mode" -> "foo-bar-minor"
				     "toggle-\\|-mode\\'" ""
                                     (symbol-name mode))))
		       " mode")))
    (setq name (replace-regexp-in-string "\\`Global-" "Global " name))
    (if (not (stringp lighter)) name
      ;; Strip leading and trailing whitespace from LIGHTER.
      (setq lighter (replace-regexp-in-string "\\`\\s-+\\|\\s-+\\'" ""
					      lighter))
      ;; Replace any (case-insensitive) matches for LIGHTER in NAME
      ;; with a literal LIGHTER.  E.g., if NAME is "Iimage mode" and
      ;; LIGHTER is " iImag", then this will produce "iImage mode".
      ;; (LIGHTER normally comes from the mode-line string passed to
      ;; define-minor-mode, and normally includes at least one leading
      ;; space.)
      (replace-regexp-in-string (regexp-quote lighter) lighter name t t))))

(defconst easy-mmode--arg-docstring
  "This is a %sminor mode.  If called interactively, toggle the
`%s' mode.  If the prefix argument is positive, enable the mode,
and if it is zero or negative, disable the mode.

If called from Lisp, toggle the mode if ARG is `toggle'.
Enable the mode if ARG is nil, omitted, or is a positive number.
Disable the mode if ARG is a negative number.

To check whether the minor mode is enabled in the current buffer,
evaluate %s.

The mode's hook is called both when the mode is enabled and when
it is disabled.")

(defun easy-mmode--mode-docstring (doc mode-pretty-name keymap-sym
                                       getter global)
  ;; If we have a doc string, and it's already complete (which we
  ;; guess at with the simple heuristic below), then just return that
  ;; as is.
  (if (and doc (string-match-p "\\bARG\\b" doc))
      doc
    ;; Compose a new doc string.
    (with-temp-buffer
      (let ((lines (if doc
                       (string-lines doc)
                     (list (format "Toggle %s on or off." mode-pretty-name)))))
        ;; Insert the first line from the doc string.
        (insert (pop lines))
        ;; Ensure that we have (only) one blank line after the first
        ;; line.
        (ensure-empty-lines)
        (while (and lines
                    (equal (car lines) ""))
          (pop lines))
        ;; Insert the doc string.
        (dolist (line lines)
          (insert line "\n"))
        (ensure-empty-lines)
        ;; Insert the boilerplate.
        (let* ((fill-prefix nil)
               (docs-fc (bound-and-true-p emacs-lisp-docstring-fill-column))
               (fill-column (if (integerp docs-fc) docs-fc 65))
               (argdoc (format
                        easy-mmode--arg-docstring
                        (if global "global " "")
                        mode-pretty-name
                        (concat
                         (if (symbolp getter) "the variable ")
                         (format "`%s'"
                                 ;; Avoid having quotes turn into pretty quotes.
                                 (string-replace "'" "\\='" (format "%S" getter)))))))
          (let ((start (point)))
            (insert argdoc)
            (when (fboundp 'fill-region) ;Don't break bootstrap!
              (fill-region start (point) 'left t))))
        ;; Finally, insert the keymap.
        (when (and (boundp keymap-sym)
                   (or (not doc)
                       (not (string-search "\\{" doc))))
          (ensure-empty-lines)
          (insert (format "\\{%s}" keymap-sym)))
        (buffer-string)))))

;;;###autoload
(defmacro define-minor-mode (mode doc &rest body)
  "Define a new minor mode MODE.
This defines the toggle command MODE and (by default) a control variable
MODE (you can override this with the :variable keyword, see below).
DOC is the documentation for the mode toggle command.

The defined mode command takes one optional (prefix) argument.
Interactively with no prefix argument, it toggles the mode.
A prefix argument enables the mode if the argument is positive,
and disables it otherwise.

When called from Lisp, the mode command toggles the mode if the
argument is `toggle', disables the mode if the argument is a
non-positive integer, and enables the mode otherwise (including
if the argument is omitted or nil or a positive integer).

If DOC is nil, give the mode command a basic doc-string
documenting what its argument does.  If the word \"ARG\" does not
appear in DOC, a paragraph is added to DOC explaining
usage of the mode argument.

BODY contains code to execute each time the mode is enabled or disabled.
  It is executed after toggling the mode, and before running MODE-hook.
  Before the actual body code, you can write keyword arguments, i.e.
  alternating keywords and values.  If you provide BODY, then you must
  provide at least one keyword argument (e.g. `:lighter nil`).
  The following special keywords are supported (other keywords are passed
  to `defcustom' if the minor mode is global):

:global GLOBAL	If non-nil specifies that the minor mode is not meant to be
		buffer-local, so don't make the variable MODE buffer-local.
		By default, the mode is buffer-local.
:init-value VAL	the initial value of the mode's variable.
		Note that the minor mode function won't be called by setting
		this option, so the value *reflects* the minor mode's natural
		initial state, rather than *setting* it.
		In the vast majority of cases it should be nil.
		Not used if you also specify :variable.
:lighter SPEC	Text displayed in the mode line when the mode is on.
:keymap MAP	Keymap bound to the mode keymap.  Defaults to `MODE-map'.
                If non-nil, it should be an unquoted variable name (whose value
                is a keymap), or an expression that returns either a keymap or
		a list of (KEY . BINDING) pairs where KEY and BINDING are
		suitable for `define-key'.  If you supply a KEYMAP argument
		that is not a symbol, this macro defines the variable MODE-map
		and gives it the value that KEYMAP specifies.
:interactive VAL  Whether this mode should be a command or not.  The default
                is to make it one; use nil to avoid that.  If VAL is a list,
                it's interpreted as a list of major modes this minor mode
                is useful in.
:variable PLACE	The location to use instead of the variable MODE to store
		the state of the mode.	This can be simply a different
		named variable, or a generalized variable.
		PLACE can also be of the form (GET . SET), where GET is
		an expression that returns the current state, and SET is
		a function that takes one argument, the new state, which should
                be assigned to PLACE.  If you specify a :variable, this function
                does not define a MODE variable (nor any of the terms used
		in :variable).
:after-hook     A single Lisp form which is evaluated after the mode hooks
                have been run.  It should not be quoted.

For example, you could write
  (define-minor-mode foo-mode \"If enabled, foo on you!\"
    :lighter \" Foo\" :require \\='foo :global t :group \\='hassle :version \"27.5\"
    ...BODY CODE...)

For backward compatibility with the Emacs<21 calling convention,
the keywords can also be preceded by the obsolete triplet
INIT-VALUE LIGHTER KEYMAP.

\(fn MODE DOC [KEYWORD VAL ... &rest BODY])"
  (declare (doc-string 2)
           (indent defun)
           (debug (&define name string-or-null-p
			   [&optional [&not keywordp] sexp
			    &optional [&not keywordp] sexp
			    &optional [&not keywordp] sexp]
			   [&rest [keywordp sexp]]
			   def-body)))

  (let* ((last-message (make-symbol "last-message"))
         (mode-name (symbol-name mode))
         (init-value nil)
         (keymap nil)
         (lighter nil)
	 (pretty-name nil)
	 (globalp nil)
	 (set nil)
	 (initialize nil)
	 (type nil)
	 (extra-args nil)
	 (extra-keywords nil)
         (variable nil)          ;The PLACE where the state is stored.
         (setter `(setq ,mode))  ;The beginning of the exp to set the mode var.
         (getter mode)           ;The exp to get the mode value.
         (modefun mode)          ;The minor mode function name we're defining.
	 (after-hook nil)
	 (hook (intern (concat mode-name "-hook")))
	 (hook-on (intern (concat mode-name "-on-hook")))
	 (hook-off (intern (concat mode-name "-off-hook")))
         (interactive t)
         (new-style
          (or (null body)
              (keywordp (car body))))
         (warnwrap (if new-style #'identity
                     (lambda (exp)
                       (macroexp-warn-and-return
                        (format-message
                         "Use keywords rather than deprecated positional arguments to `define-minor-mode'")
                        exp))))
	 keyw keymap-sym tmp)

    ;; Allow BODY to start with the old INIT-VALUE LIGHTER KEYMAP triplet.
    (unless new-style
      (setq init-value (pop body))
      (unless (keywordp (car body))
        (setq lighter (pop body))
        (unless (keywordp (car body))
          (setq keymap (pop body)))))

    ;; Check keys.
    (while (keywordp (setq keyw (car body)))
      (setq body (cdr body))
      (pcase keyw
	(:init-value (setq init-value (pop body)))
        (:lighter (setq lighter (pop body)))
	(:global (setq globalp (pop body))
                 (when (and globalp (symbolp mode))
                   (setq setter `(setq-default ,mode))
                   (setq getter `(default-value ',mode))))
	(:extra-args (setq extra-args (pop body)))
	(:set (setq set (list :set (pop body))))
	(:initialize (setq initialize (list :initialize (pop body))))
	(:type (setq type (list :type (pop body))))
	(:keymap (setq keymap (pop body)))
	(:interactive (setq interactive (pop body)))
        (:variable (setq variable (pop body))
                   (if (not (and (setq tmp (cdr-safe variable))
                                 (or (symbolp tmp)
                                     (functionp tmp))))
                       ;; PLACE is not of the form (GET . SET).
                       (progn
                         (setq setter `(setf ,variable))
                         (setq getter variable))
                     (setq getter (car variable))
                     (setq setter `(funcall #',(cdr variable)))))
	(:after-hook (setq after-hook (pop body)))
	(_ (push keyw extra-keywords) (push (pop body) extra-keywords))))

    (setq pretty-name (easy-mmode-pretty-mode-name mode lighter))
    (setq keymap-sym (if (and keymap (symbolp keymap)) keymap
		       (intern (concat mode-name "-map"))))

    (unless set (setq set '(:set #'custom-set-minor-mode)))

    (unless initialize
      (setq initialize '(:initialize #'custom-initialize-default)))

    ;; TODO? Mark booleans as safe if booleanp?  Eg abbrev-mode.
    (unless type (setq type '(:type 'boolean)))

    `(progn
       ;; Define the variable to enable or disable the mode.
       ,(cond
         ;; If :variable is specified, then the var will be
         ;; declared elsewhere.
         (variable nil)
         ((not globalp)
          `(progn
             :autoload-end
             (defvar-local ,mode ,init-value
               ,(concat (format "Non-nil if %s is enabled.\n" pretty-name)
                        (internal--format-docstring-line
                         "Use the command `%s' to change this variable." mode)))))
         (t
	  (let ((base-doc-string
                 (concat "Non-nil if %s is enabled.
See the `%s' command
for a description of this minor mode."
                         (if body "
Setting this variable directly does not take effect;
either customize it (see the info node `Easy Customization')
or call the function `%s'."))))
	    `(defcustom ,mode ,init-value
	       ,(format base-doc-string pretty-name mode mode)
	       ,@set
	       ,@initialize
	       ,@type
               ,@(nreverse extra-keywords)))))

       ;; The actual function.
       ,(funcall
         warnwrap
         `(defun ,modefun (&optional arg ,@extra-args)
            ,(easy-mmode--mode-docstring doc pretty-name keymap-sym
                                         getter globalp)
            ,(when interactive
	       ;; Use `toggle' rather than (if ,mode 0 1) so that using
	       ;; repeat-command still does the toggling correctly.
               (if (consp interactive)
                   `(interactive
                     (list (if current-prefix-arg
                               (prefix-numeric-value current-prefix-arg)
                             'toggle))
                     ,@interactive)
	         '(interactive
                   (list (if current-prefix-arg
                             (prefix-numeric-value current-prefix-arg)
                           'toggle)))))
	    (let ((,last-message (current-message)))
              (,@setter
               (cond ((eq arg 'toggle)
                      (not ,getter))
                     ((and (numberp arg)
                           (< arg 1))
                      nil)
                     (t
                      t)))
              ;; Keep minor modes list up to date.
              ,@(if globalp
                    ;; When running this byte-compiled code in earlier
                    ;; Emacs versions, these variables may not be defined
                    ;; there.  So check defensively, even if they're
                    ;; always defined in Emacs 28 and up.
                    `((when (boundp 'global-minor-modes)
                        (setq global-minor-modes
                              (delq ',modefun global-minor-modes))
                        (when ,getter
                          (push ',modefun global-minor-modes))))
                  ;; Ditto check.
                  `((when (boundp 'local-minor-modes)
                      (setq local-minor-modes
                            (delq ',modefun local-minor-modes))
                      (when ,getter
                        (push ',modefun local-minor-modes)))))
              ,@body
              ;; The on/off hooks are here for backward compatibility only.
              (run-hooks ',hook (if ,getter ',hook-on ',hook-off))
              (if (called-interactively-p 'any)
                  (progn
                    ,(if (and globalp (not variable))
                         `(customize-mark-as-set ',mode))
                    ;; Avoid overwriting a message shown by the body,
                    ;; but do overwrite previous messages.
                    (unless (and (current-message)
                                 (not (equal ,last-message
                                             (current-message))))
                      (let ((local ,(if globalp "" " in current buffer")))
			(message "%s %sabled%s" ,pretty-name
			         (if ,getter "en" "dis") local)))))
	      ,@(when after-hook `(,after-hook)))
	    (force-mode-line-update)
	    ;; Return the new setting.
	    ,getter))

       ;; Autoloading a define-minor-mode autoloads everything
       ;; up-to-here.
       :autoload-end

       (defvar ,hook nil)
       (unless (get ',hook 'variable-documentation)
         (put ',hook 'variable-documentation
              ,(format "Hook run after entering or leaving `%s'.
No problems result if this variable is not bound.
`add-hook' automatically binds it.  (This is true for all hook variables.)"
                       modefun)))
       ;; Allow using `M-x customize-variable' on the hook.
       (put ',hook 'custom-type 'hook)
       (put ',hook 'standard-value (list nil))

       ;; Define the minor-mode keymap.
       ,(unless (symbolp keymap)	;nil is also a symbol.
	  `(defvar ,keymap-sym
	     (let ((m ,keymap))
	       (cond ((keymapp m) m)
                     ;; FIXME: `easy-mmode-define-keymap' is obsolete,
                     ;; so this form should also be obsolete somehow.
		     ((listp m)
                      (with-suppressed-warnings ((obsolete
                                                  easy-mmode-define-keymap))
                        (easy-mmode-define-keymap m)))
		     (t (error "Invalid keymap %S" m))))
	     ,(format "Keymap for `%s'." mode-name)))

       ,(let ((modevar (pcase getter (`(default-value ',v) v) (_ getter))))
          (if (not (symbolp modevar))
              (if (or lighter keymap)
                  (error ":lighter and :keymap unsupported with mode expression %S" getter))
            `(with-no-warnings
               (add-minor-mode ',modevar ',lighter
                               ,(if keymap keymap-sym
                                  `(if (boundp ',keymap-sym) ,keymap-sym))
                               nil
                               ,(unless (eq mode modefun) `',modefun))))))))

;;;
;;; make global minor mode
;;;

;;;###autoload
(defmacro define-globalized-minor-mode (global-mode mode turn-on &rest body)
  "Make a global mode GLOBAL-MODE corresponding to buffer-local minor MODE.
TURN-ON is a function that will be called with no args in every buffer
and that should try to turn MODE on if applicable for that buffer.

Each of KEY VALUE is a pair of CL-style keyword arguments.
The :predicate key specifies in which major modes should the
globalized minor mode be switched on.  The value should be t (meaning
switch on the minor mode in all major modes), nil (meaning don't
switch on in any major mode), a list of modes (meaning switch on only
in those modes and their descendants), or a list (not MODES...),
meaning switch on in any major mode except MODES.  The value can also
mix all of these forms, see the Info node `(elisp)Defining Minor Modes'
for details.  The :predicate key causes the macro to create a user option
named the same as MODE, but ending with \"-modes\" instead of \"-mode\".
That user option can then be used to customize in which modes this
globalized minor mode will be switched on.
As the minor mode defined by this function is always global, any
:global keyword is ignored.
Other keywords have the same meaning as in `define-minor-mode',
which see.  In particular, :group specifies the custom group.
The most useful keywords are those that are passed on to the `defcustom'.
It normally makes no sense to pass the :lighter or :keymap keywords
to `define-globalized-minor-mode', since these are usually passed to
the buffer-local version of the minor mode.

BODY contains code to execute each time the mode is enabled or disabled.
It is executed after toggling the mode, and before running
GLOBAL-MODE-hook.

If MODE's set-up depends on the major mode in effect when it was
enabled, then disabling and reenabling MODE should make MODE work
correctly with the current major mode.  This is important to
prevent problems with derived modes, that is, major modes that
call another major mode in their body.

When a major mode is initialized, MODE is actually turned on just
after running the major mode's hook.  However, MODE is not turned
on if the hook has explicitly disabled it.

\(fn GLOBAL-MODE MODE TURN-ON [KEY VALUE]... BODY...)"
  (declare (doc-string 2) (indent defun))
  (let* ((global-mode-name (symbol-name global-mode))
	 (mode-name (symbol-name mode))
	 (pretty-name (easy-mmode-pretty-mode-name mode))
	 (pretty-global-name (easy-mmode-pretty-mode-name global-mode))
	 (group nil)
	 (extra-keywords nil)
         (MODE-variable mode)
	 (MODE-enable-in-buffer
	  (intern (concat global-mode-name "-enable-in-buffer")))
	 (minor-MODE-hook (intern (concat mode-name "-hook")))
	 (MODE-set-explicitly (intern (concat mode-name "-set-explicitly")))
	 (MODE-major-mode (intern (concat (symbol-name mode) "-major-mode")))
         (MODE-predicate (intern (concat (replace-regexp-in-string
                                          "-mode\\'" "" global-mode-name)
                                         "-modes")))
         (turn-on-function `#',turn-on)
	 keyw predicate)

    ;; Check keys.
    (while (keywordp (setq keyw (car body)))
      (pop body)
      (pcase keyw
        (:group (setq group (nconc group (list :group (pop body)))))
        (:global (pop body))
        (:variable (setq MODE-variable (pop body)))
        (:predicate
         (setq predicate (list (pop body)))
         (setq turn-on-function
               `(lambda ()
                  (require 'easy-mmode)
                  (when (easy-mmode--globalized-predicate-p ,MODE-predicate)
                    (funcall ,turn-on-function)))))
        (_ (push keyw extra-keywords) (push (pop body) extra-keywords))))

    `(progn
       (progn
         (put ',global-mode 'globalized-minor-mode t)
         :autoload-end
         (defvar-local ,MODE-major-mode nil)
         ,@(when predicate `((defvar ,MODE-predicate))))
       ;; The actual global minor-mode
       (define-minor-mode ,global-mode
         ,(concat (format "Toggle %s in many buffers.\n" pretty-name)
                  (internal--format-docstring-line
                   "Specifically, %s is enabled in all buffers where `%S' would do it."
                   pretty-name turn-on)
                  "\n\n"
                  (internal--format-docstring-line
                   (concat "With prefix ARG, enable %s if ARG is positive; "
                           "otherwise, disable it.")
                   pretty-global-name)
                  "\n\n"
                  "If called from Lisp, toggle the mode if ARG is `toggle'.
Enable the mode if ARG is nil, omitted, or is a positive number.
Disable the mode if ARG is a negative number.\n\n"
                  (internal--format-docstring-line
                   "See `%s' for more information on %s."
                   mode pretty-name)
                  (if predicate
                      (concat
                       "\n\n"
                       (internal--format-docstring-line
                        "`%s' is used to control which modes this minor mode is used in."
                        MODE-predicate))
                    ""))
         :global t ,@group ,@(nreverse extra-keywords)

	 ;; Setup hook to handle future mode changes and new buffers.
	 (if ,global-mode
	     (add-hook 'after-change-major-mode-hook
		       #',MODE-enable-in-buffer)
	   (remove-hook 'after-change-major-mode-hook #',MODE-enable-in-buffer))

	 ;; Go through existing buffers.
	 (dolist (buf (buffer-list))
	   (with-current-buffer buf
             (if ,global-mode (funcall ,turn-on-function)
               (when ,mode (,mode -1)))))
         ,@body)

       ,(when predicate
          `(defcustom ,MODE-predicate ,(car predicate)
             ,(format "Which major modes `%s' is switched on in.
This variable can be either t (all major modes), nil (no major modes),
or a list of modes and (not modes) to switch use this minor mode or
not.  For instance

  (c-mode (not message-mode mail-mode) text-mode)

means \"use this mode in all modes derived from `c-mode', don't use in
modes derived from `message-mode' or `mail-mode', but do use in other
modes derived from `text-mode'\".  An element with value t means \"use\"
and nil means \"don't use\".  There's an implicit nil at the end of the
list."
                      mode)
             :type '(choice
                     (const :tag "Enable in all major modes" t)
                     (const :tag "Don't enable in any major mode" nil)
                     (repeat
                      :tag "Rules (earlier takes precedence)..."
                      (choice
                       (const :tag "Enable in all (other) modes" t)
                       (const :tag "Don't enable in any (other) mode" nil)
                       (symbol :value fundamental-mode
                               :tag "Enable in major mode")
                       (cons :tag "Don't enable in major modes"
                             (const :tag "Don't enable in..." not)
                             (repeat (symbol :value fundamental-mode
                                             :tag "Major mode"))))))
             ,@group))

       ;; Autoloading define-globalized-minor-mode autoloads everything
       ;; up-to-here.
       :autoload-end

       ;; MODE-set-explicitly is set in MODE-set-explicitly and cleared by
       ;; kill-all-local-variables.
       (defvar-local ,MODE-set-explicitly nil)
       (defun ,MODE-set-explicitly ()
         (setq ,MODE-set-explicitly t))
       (put ',MODE-set-explicitly 'definition-name ',global-mode)

       ;; A function which checks whether MODE has been disabled in the major
       ;; mode hook which has just been run.
       (add-hook ',minor-MODE-hook #',MODE-set-explicitly)

       ;; The function that calls TURN-ON in the current buffer.
       (defun ,MODE-enable-in-buffer ()
         (unless ,MODE-set-explicitly
           (unless (eq ,MODE-major-mode major-mode)
             (if ,MODE-variable
                 (progn
                   (,mode -1)
                   (funcall ,turn-on-function))
               (funcall ,turn-on-function))))
         (setq ,MODE-major-mode major-mode))
       (put ',MODE-enable-in-buffer 'definition-name ',global-mode))))

(defun easy-mmode--globalized-predicate-p (predicate)
  (cond
   ((eq predicate t)
    t)
   ((eq predicate nil)
    nil)
   ((listp predicate)
    ;; Legacy support for (not a b c).
    (when (eq (car predicate) 'not)
      (setq predicate (nconc (mapcar (lambda (e) (list 'not e))
                                     (cdr predicate))
                             (list t))))
    (catch 'found
      (dolist (elem predicate)
        (cond
         ((eq elem t)
          (throw 'found t))
         ((eq elem nil)
          (throw 'found nil))
         ((and (consp elem)
               (eq (car elem) 'not))
          (when (derived-mode-p (cdr elem))
            (throw 'found nil)))
         ((symbolp elem)
          (when (derived-mode-p elem)
            (throw 'found t)))))))))

;;;
;;; easy-mmode-defmap
;;;

(defun easy-mmode-set-keymap-parents (m parents)
  (set-keymap-parent
   m (if (cdr parents) (make-composed-keymap parents) (car parents))))

;;;###autoload
(defun easy-mmode-define-keymap (bs &optional name m args)
  "Return a keymap built from bindings BS.
BS must be a list of (KEY . BINDING) where
KEY and BINDINGS are suitable for `define-key'.
Optional NAME is passed to `make-sparse-keymap'.
Optional map M can be used to modify an existing map.
ARGS is a list of additional keyword arguments.

Valid keywords and arguments are:

  :name      Name of the keymap; overrides NAME argument.
  :dense     Non-nil for a dense keymap.
  :inherit   Parent keymap.
  :group     Ignored.
  :suppress  Non-nil to call `suppress-keymap' on keymap,
             `nodigits' to suppress digits as prefix arguments."
  (declare (obsolete define-keymap "29.1"))
  (let (inherit dense suppress)
    (while args
      (let ((key (pop args))
	    (val (pop args)))
	(pcase key
	  (:name (setq name val))
	  (:dense (setq dense val))
	  (:inherit (setq inherit val))
	  (:suppress (setq suppress val))
	  (:group)
	 (_ (message "Unknown argument %s in defmap" key)))))
    (unless (keymapp m)
      (setq bs (append m bs))
      (setq m (if dense (make-keymap name) (make-sparse-keymap name))))
    (when suppress
      (suppress-keymap m (eq suppress 'nodigits)))
    (dolist (b bs)
      (let ((keys (car b))
	    (binding (cdr b)))
	(dolist (key (if (consp keys) keys (list keys)))
	  (cond
	   ((symbolp key)
	    (substitute-key-definition key binding m global-map))
	   ((null binding)
	    (unless (keymapp (lookup-key m key)) (define-key m key binding)))
	   ((let ((o (lookup-key m key)))
	      (or (null o) (numberp o) (eq o 'undefined)))
	    (define-key m key binding))))))
    (cond
     ((keymapp inherit) (set-keymap-parent m inherit))
     ((consp inherit) (easy-mmode-set-keymap-parents m inherit)))
    m))

;;;###autoload
(defmacro easy-mmode-defmap (m bs doc &rest args)
  "Define a constant M whose value is the result of `easy-mmode-define-keymap'.
The M, BS, and ARGS arguments are as per that function.  DOC is
the constant's documentation.

This macro is deprecated; use `defvar-keymap' instead."
  (declare (doc-string 3) (indent 1) (obsolete defvar-keymap "29.1"))
  `(defconst ,m
     (easy-mmode-define-keymap ,bs nil (if (boundp ',m) ,m) ,(cons 'list args))
     ,doc))


;;;
;;; easy-mmode-defsyntax
;;;

(defun easy-mmode-define-syntax (css args)
  (let ((st (make-syntax-table (plist-get args :copy)))
	(parent (plist-get args :inherit)))
    (dolist (cs css)
      (let ((char (car cs))
	    (syntax (cdr cs)))
	(if (sequencep char)
	    (mapc (lambda (c) (modify-syntax-entry c syntax st)) char)
	  (modify-syntax-entry char syntax st))))
    (if parent (set-char-table-parent
		st (if (symbolp parent) (symbol-value parent) parent)))
    st))

;;;###autoload
(defmacro easy-mmode-defsyntax (st css doc &rest args)
  "Define variable ST as a syntax-table.
CSS contains a list of syntax specifications of the form (CHAR . SYNTAX)."
  (declare (doc-string 3) (indent 1))
  `(progn
     (autoload 'easy-mmode-define-syntax "easy-mmode")
     (defconst ,st (easy-mmode-define-syntax ,css ,(cons 'list args)) ,doc)))



;;;
;;; easy-mmode-define-navigation
;;;

(defun easy-mmode--prev (re name count &optional endfun narrowfun)
  "Go to the COUNT'th previous occurrence of RE.

If none, error with NAME.

ENDFUN and NARROWFUN are treated like in `easy-mmode-define-navigation'."
  (unless count (setq count 1))
  (if (< count 0) (easy-mmode--next re name (- count) endfun narrowfun)
    (let ((re-narrow (and narrowfun (prog1 (buffer-narrowed-p) (widen)))))
      ;; If point is inside a match for RE, move to its beginning like
      ;; `backward-sexp' and other movement commands.
      (when (and (not (zerop count))
                 (save-excursion
                   ;; Make sure we're out of the current match if any.
                   (goto-char (if (re-search-backward re nil t 1)
                                  (match-end 0) (point-min)))
                   (re-search-forward re nil t 1))
                 (< (match-beginning 0) (point) (match-end 0)))
        (goto-char (match-beginning 0))
        (setq count (1- count)))
      (unless (re-search-backward re nil t count)
        (user-error "No previous %s" name))
      (when re-narrow (funcall narrowfun)))))

(defun easy-mmode--next (re name count &optional endfun narrowfun)
  "Go to the next COUNT'th occurrence of RE.

If none, error with NAME.

ENDFUN and NARROWFUN are treated like in `easy-mmode-define-navigation'."
  (unless count (setq count 1))
  (if (< count 0) (easy-mmode--prev re name (- count) endfun narrowfun)
    (if (looking-at re) (setq count (1+ count)))
    (let ((re-narrow (and narrowfun (prog1 (buffer-narrowed-p) (widen)))))
      (if (not (re-search-forward re nil t count))
          (if (looking-at re)
              (goto-char (or (if endfun (funcall endfun)) (point-max)))
            (user-error "No next %s" name))
        (goto-char (match-beginning 0))
        (when (and (eq (current-buffer) (window-buffer))
                   (called-interactively-p 'interactive))
          (let ((endpt (or (save-excursion
                             (if endfun (funcall endfun)
                               (re-search-forward re nil t 2)))
                           (point-max))))
            (unless (pos-visible-in-window-p endpt nil t)
              (let ((ws (window-start)))
                (recenter '(0))
                (if (< (window-start) ws)
                    ;; recenter scrolled in the wrong direction!
                    (set-window-start nil ws)))))))
      (when re-narrow (funcall narrowfun)))))

(defmacro easy-mmode-define-navigation (base re &optional name endfun narrowfun
                                             &rest body)
  "Define BASE-next and BASE-prev to navigate in the buffer.
RE determines the places the commands should move point to.
NAME should describe the entities matched by RE.  It is used to build
  the docstrings of the two functions.
BASE-next also tries to make sure that the whole entry is visible by
  searching for its end (by calling ENDFUN if provided or by looking for
  the next entry) and recentering if necessary.
ENDFUN should return the end position (with or without moving point).
NARROWFUN non-nil means to check for narrowing before moving, and if
found, do `widen' first and then call NARROWFUN with no args after moving.
BODY is executed after moving to the destination location."
  (declare (indent 5) (debug (exp exp exp def-form def-form def-body)))
  (let* ((base-name (symbol-name base))
	 (prev-sym (intern (concat base-name "-prev")))
	 (next-sym (intern (concat base-name "-next")))
         (endfun (when endfun `#',endfun))
         (narrowfun (when narrowfun `#',narrowfun)))
    (unless name (setq name base-name))
    `(progn
       (defun ,next-sym (&optional count)
	 ,(format "Go to the next COUNT'th %s.
Interactively, COUNT is the prefix numeric argument, and defaults to 1." name)
	 (interactive "p")
         (easy-mmode--next ,re ,name count ,endfun ,narrowfun)
         ,@body)
       (put ',next-sym 'definition-name ',base)
       (defun ,prev-sym (&optional count)
	 ,(format "Go to the previous COUNT'th %s.
Interactively, COUNT is the prefix numeric argument, and defaults to 1." name)
	 (interactive "p")
         (easy-mmode--prev ,re ,name count ,endfun ,narrowfun)
         ,@body)
       (put ',prev-sym 'definition-name ',base))))

;; When deleting these, also delete them from loaddefs-gen.el.
;;;###autoload
(define-obsolete-function-alias 'easy-mmode-define-minor-mode #'define-minor-mode "30.1")
;;;###autoload
(define-obsolete-function-alias 'easy-mmode-define-global-mode #'define-globalized-minor-mode "30.1")
;;;###autoload
(define-obsolete-function-alias 'define-global-minor-mode
  #'define-globalized-minor-mode "31.1")

(provide 'easy-mmode)

;;; easy-mmode.el ends here
