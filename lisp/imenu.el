;;; imenu.el --- framework for mode-specific buffer indexes  -*- lexical-binding: t -*-

;; Copyright (C) 1994-1998, 2001-2025 Free Software Foundation, Inc.

;; Author: Ake Stenhoff <etxaksf@aom.ericsson.se>
;;         Lars Lindberg <lli@sypro.cap.se>
;; Maintainer: emacs-devel@gnu.org
;; Created: 8 Feb 1994
;; Keywords: tools convenience

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

;; Purpose of this package:
;;   To present a framework for mode-specific buffer indexes.
;;   A buffer index is an alist of names and buffer positions.
;;   For instance all functions in a C-file and their positions.
;;
;;   It is documented in the Emacs Lisp manual.
;;
;; How it works:

;;   A mode-specific function is called to generate the index.  It is
;;   then presented to the user, who can choose from this index.

;;; History:
;;  Thanks go to
;;  [simon] - Simon Leinen simon@lia.di.epfl.ch
;;  [dean] - Dean Andrews ada@unison.com
;;  [alon] - Alon Albert al@mercury.co.il
;;  [greg] - Greg Thompson gregt@porsche.visix.COM
;;  [wolfgang] - Wolfgang Bangerth zcg51122@rpool1.rus.uni-stuttgart.de
;;  [kai] - Kai Grossjohann grossjoh@linus.informatik.uni-dortmund.de
;;  [david] - David M. Smith dsmith@stats.adelaide.edu.au
;;  [christian] - Christian Egli Christian.Egli@hcsd.hac.com
;;  [karl] - Karl Fogel kfogel@floss.life.uiuc.edu

;;; Code:

(require 'cl-lib)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Customizable variables
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defgroup imenu nil
  "Mode-specific buffer indexes."
  :group 'matching
  :group 'frames
  :group 'convenience
  :link '(custom-manual "(elisp)Imenu"))

(defcustom imenu-use-markers t
  "Non-nil means use markers instead of integers for Imenu buffer positions.

Setting this to nil makes Imenu work a little faster but editing the
buffer will make the generated index positions wrong.

This might not yet be honored by all index-building functions."
  :type 'boolean)

(defcustom imenu-max-item-length 60
  "If a number, truncate Imenu entries to that length."
  :type '(choice integer
                 (const :tag "Unlimited")))

(defcustom imenu-auto-rescan nil
  "Non-nil means Imenu should always rescan the buffers."
  :type 'boolean)

(defcustom imenu-auto-rescan-maxout 600000
  "Imenu auto-rescan is disabled in buffers larger than this size (in bytes).
Also see `imenu-max-index-time'."
  :type 'natnum
  :version "26.2")

(defcustom imenu-use-popup-menu 'on-mouse
  "Use a popup menu rather than a minibuffer prompt.
If nil, always use a minibuffer prompt.
If t, always use a popup menu,
If `on-mouse' use a popup menu when `imenu' was invoked with the mouse."
  :type '(choice (const :tag "On Mouse" on-mouse)
		 (const :tag "Never" nil)
                 (other :tag "Always" t)))

(defcustom imenu-eager-completion-buffer t
  "If nil, eagerly pop up the completion buffer."
  :type 'boolean
  :version "22.1")

(defcustom imenu-after-jump-hook nil
  "Hooks called after jumping to a place in the buffer.

Useful things to use here include `reposition-window', `recenter', and
\(lambda () (recenter 0)) to show at top of screen."
  :type 'hook)

;;;###autoload
(defcustom imenu-sort-function nil
  "The function to use for sorting the index mouse-menu.

Affects only the mouse index menu.  If you want to change
the sorting order of completions, you can customize
the option `completion-category-overrides' and set
`display-sort-function' for the category `imenu'.

Set this to nil if you don't want any sorting (faster).
The items in the menu are then presented in the order they were found
in the buffer.

Set it to `imenu--sort-by-name' if you want alphabetic sorting.

The function should take two arguments and return t if the first
element should come before the second.  The arguments are cons cells;
\(NAME . POSITION).  Look at `imenu--sort-by-name' for an example."
  :type '(choice (const :tag "No sorting" nil)
		 (const :tag "Sort by name" imenu--sort-by-name)
                 (function :tag "Another function")))

(defcustom imenu-max-items 25
  "Maximum number of elements in a mouse menu for Imenu."
  :type 'natnum)

(defcustom imenu-space-replacement "."
  "The replacement string for spaces in index names.
Used when presenting the index in a completion buffer to make the
names work as tokens."
  :type '(choice string (const nil)))

(defcustom imenu-level-separator ":"
  "The separator between index names of different levels.
Used for flattening nested indexes with name concatenation."
  :type 'string)

(defcustom imenu-flatten nil
  "Whether to flatten the list of sections in an imenu or show it nested.
If nil, use nested indexes.
If the value is `prefix', pop up the completion buffer with a
flattened menu where section names are prepended to completion
candidates as prefixes.
If the value is `annotation', annotate each completion candidate
with a suffix that is the section name to which it belongs.
If the value is `group', split completion candidates into groups
according to the sections.
Any other value is treated as `prefix'.

Since the values `annotation' and `group' rely on text properties,
you can use them only by selecting candidates from the completions
buffer, not by typing in the minibuffer.  This also means that
if you use `minibuffer-next-completion' (`M-<down>') to select
a completion while point stays in the minibuffer, you need
to customize `minibuffer-completion-auto-choose' to nil that
doesn't insert completion candidates to the minibuffer.
Also note that for using the value `group' you need to customize
`completions-group' to the value t, and `completions-format'
to the value `vertical'.

The value of `imenu-level-separator', a string, is used to separate
names from different flattened levels, such as section names, from the
names of completion candidates."
  :type '(choice (const :tag "Show nested list" nil)
                 (const :tag "Flat list with sections as prefix" prefix)
                 (const :tag "Flat list annotated with sections" annotation)
                 (const :tag "Flat list grouped by sections" group))
  :version "30.1")

(defcustom imenu-generic-skip-comments-and-strings t
  "When non-nil, ignore text inside comments and strings.
Only affects `imenu-default-create-index-function' (and any
alternative implementation of `imenu-create-index-function' that
uses `imenu--generic-function')."
  :type 'boolean
  :version "24.4")

(defcustom imenu-max-index-time 5
  "Max time to use when creating imenu indices."
  :type 'number
  :version "28.1")

(defcustom imenu-allow-duplicate-menu-items t
  "Non-nil means that Imenu can include duplicate menu items.
For example, if the buffer contains multiple definitions of function
`foo' then a menu item is included for each of them.
Otherwise, only the first such definition is accessible from the menu.

This option applies only to an Imenu menu, not also to the use of
command `imenu', which uses `completing-read' to read a menu item.
The use of that command doesn't allow duplicate items."
  :type 'boolean
  :version "31.1")

;;;###autoload
(defvar-local imenu-generic-expression nil
  "List of definition matchers for creating an Imenu index.
Each element of this list should have the form

  (MENU-TITLE REGEXP INDEX [FUNCTION] [ARGUMENTS...])

MENU-TITLE should be nil (in which case the matches for this
element are put in the top level of the buffer index) or a
string (which specifies the title of a submenu into which the
matches are put).
REGEXP is a regular expression matching a definition construct
which is to be displayed in the menu.  REGEXP may also be a
function of no arguments.  If REGEXP is a function, it is
expected to search backwards, return non-nil if it finds a
definition construct, and set `match-data' for that construct.
INDEX is an integer specifying which subexpression of REGEXP
matches the definition's name; this subexpression is displayed as
the menu item.
FUNCTION, if present, specifies a function to call when the index
item is selected by the user.  This function is called with
arguments consisting of the item name, the buffer position, and
the ARGUMENTS.

The variable `imenu-case-fold-search' determines whether or not
the regexp matches are case sensitive, and `imenu-syntax-alist'
can be used to alter the syntax table for the search.

If non-nil this pattern is passed to `imenu--generic-function' to
create a buffer index.

For example, see the value of `fortran-imenu-generic-expression'
used by `fortran-mode' with `imenu-syntax-alist' set locally so that
characters which normally have \"symbol\" syntax are considered to have
\"word\" syntax during matching.")
;;;###autoload(put 'imenu-generic-expression 'risky-local-variable t)

;;;; Hooks

;;;###autoload
(defvar-local imenu-create-index-function 'imenu-default-create-index-function
  "The function to use for creating an index alist of the current buffer.

It should be a function that takes no arguments and returns
an index alist of the current buffer.  The function is
called within a `save-excursion'.

See `imenu--index-alist' for the format of the buffer index alist.")

;;;###autoload
(defvar-local imenu-submenus-on-top t
  "Flag specifying whether items with sublists should be kept at top.

For some indexes, such as those describing sections in a document, it
makes sense to keep their original order even in the menubar.")

;;;###autoload
(defvar-local imenu-prev-index-position-function 'beginning-of-defun
  "Function for finding the next index position.

If `imenu-create-index-function' is set to
`imenu-default-create-index-function', then you must set this variable
to a function that will find the next index, looking backwards in the
file.

The function should leave point at the place to be connected to the
index and it should return nil when it doesn't find another index.")

;;;###autoload
(defvar-local imenu-extract-index-name-function nil
  "Function for extracting the index item name, given a position.

This function is called after `imenu-prev-index-position-function'
finds a position for an index item, with point at that position.
It should return the name for that index item.")

;;;###autoload
(defvar-local imenu-name-lookup-function nil
  "Function to compare string with index item.

This function will be called with two strings, and should return
non-nil if they match.

If nil, comparison is done with `string='.
Set this to some other function for more advanced comparisons,
such as \"begins with\" or \"name matches and number of
arguments match\".")

;;;###autoload
(defvar-local imenu-default-goto-function 'imenu-default-goto-function
  "The default function called when selecting an Imenu item.
The function in this variable is called when selecting a normal index-item.")


(defun imenu--subalist-p (item)
  (and (consp item)
       (consp (cdr item))
       (listp (cadr item))
       (not (functionp (cadr item)))))

(defmacro imenu-progress-message (_prevpos &optional _relpos _reverse)
  "This macro is obsolete and does nothing."
  (declare (obsolete nil "28.1"))
;; Made obsolete/empty, as computers are now faster than the eye, and
;; it had problems updating the messages correctly, and could shadow
;; more important messages/prompts in the minibuffer.  KFS 2004-10-27.
)


;;;
;;; Lisp
;;;

(define-error 'imenu-unavailable "imenu unavailable")

(defun imenu-unavailable-error (format &rest args)
  (signal 'imenu-unavailable
          (list (apply #'format-message format args))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Internal variables
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; The item to use in the index for rescanning the buffer.
(defconst imenu--rescan-item '("*Rescan*" . -99))

;; The latest buffer index.
(defvar-local imenu--index-alist nil
  "The buffer index alist computed for this buffer in Imenu.

Simple elements in the alist look like (INDEX-NAME . POSITION).
POSITION is the buffer position of the item; to go to the item
is simply to move point to that position.

POSITION is passed to `imenu-default-goto-function', so it can be
a non-number if that variable has been changed (e.g. Semantic
uses overlays for POSITIONs).

Special elements look like
\(INDEX-NAME POSITION FUNCTION ARGUMENTS...).
To \"go to\" a special element means applying FUNCTION to
INDEX-NAME, POSITION, and the ARGUMENTS.

A nested sub-alist element looks like (INDEX-NAME . SUB-ALIST).
The function `imenu--subalist-p' tests an element and returns t
if it is a sub-alist.

There is one simple element with negative POSITION; selecting that
element recalculates the buffer's index alist.")
;;;###autoload(put 'imenu--index-alist 'risky-local-variable t)

(defvar-local imenu--last-menubar-index-alist nil
  "The latest buffer index alist used to update the menu bar menu.")

(defvar imenu--history-list nil
  ;; Making this buffer local caused it not to work!
  "History list for `imenu-choose-buffer-index'.")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Internal support functions
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun imenu--sort-by-name (item1 item2)
  "Comparison function to sort items depending on their index name.
An item looks like (NAME . POSITION)."
  (string-lessp (car item1) (car item2)))

(defun imenu--sort-by-position (item1 item2)
  "Comparison function to sort items depending on their position.
Return non-nil if and only if ITEM1's position is lower than ITEM2's
position."
  (if (listp (cdr item1))
      (< (cadr item1) (cadr item2))
    (< (cdr item1) (cdr item2))))

(defun imenu--relative-position (&optional reverse)
  "Support function to calculate relative position in buffer.
Beginning of buffer is 0 and end of buffer is 100
If REVERSE is non-nil then the beginning is 100 and the end is 0."
  (let ((pos (point))
	(total (buffer-size)))
    (and reverse (setq pos (- total pos)))
    (floor (* 100.0 (1- pos)) (max total 1))))

(defun imenu--split (list n)
  "Split LIST into sublists of max length N.
Example (imenu--split \\='(1 2 3 4 5 6 7 8) 3) => ((1 2 3) (4 5 6) (7 8))
The returned list DOES NOT share structure with LIST."
  (let ((remain list)
	(result '())
	(sublist '())
	(i 0))
    (while remain
      (push (pop remain) sublist)
      (incf i)
      (and (= i n)
	   ;; We have finished a sublist
	   (progn (push (nreverse sublist) result)
		  (setq i 0)
		  (setq sublist '()))))
    ;; There might be a sublist (if the length of LIST mod n is != 0)
    ;; that has to be added to the result list.
    (and sublist
	 (push (nreverse sublist) result))
    (nreverse result)))

(defun imenu--split-menu (menulist title)
  "Split the alist MENULIST into a nested alist, if it is long enough.
In any case, add TITLE to the front of the alist.
If IMENU--RESCAN-ITEM is present in MENULIST, it is moved to the
beginning of the returned alist.
The returned alist DOES NOT share structure with MENULIST."
  (let ((menulist (copy-sequence menulist))
        keep-at-top)
    (if (memq imenu--rescan-item menulist)
	(setq keep-at-top (list imenu--rescan-item)
	      menulist (delq imenu--rescan-item menulist)))
    (if imenu-submenus-on-top
        (dolist (item menulist)
          (when (imenu--subalist-p item)
	    (push item keep-at-top)
	    (setq menulist (delq item menulist)))))
    (if imenu-sort-function
	(setq menulist (sort menulist imenu-sort-function)))
    (if (> (length menulist) imenu-max-items)
	(setq menulist
	      (mapcar
	       (lambda (menu)
		 (cons (format "From: %s" (caar menu)) menu))
	       (imenu--split menulist imenu-max-items))))
    (cons title
	  (nconc (nreverse keep-at-top) menulist))))

(defun imenu--split-submenus (alist)
  "Split up each long alist that are nested within ALIST into nested alists.
Return a split and sorted copy of ALIST.  The returned alist DOES
NOT share structure with ALIST."
  (mapcar (lambda (elt)
            (if (imenu--subalist-p elt)
                (imenu--split-menu (cdr elt) (car elt))
              elt))
	  alist))

(defun imenu--truncate-items (menulist)
  "Truncate all strings in MENULIST to `imenu-max-item-length'."
  (mapc (lambda (item)
	  ;; Truncate if necessary.
	  (when (and (numberp imenu-max-item-length)
		     (> (length (car item)) imenu-max-item-length))
	    (setcar item (substring (car item) 0 imenu-max-item-length)))
	  (when (imenu--subalist-p item)
	    (imenu--truncate-items (cdr item))))
	menulist))

(defun imenu--make-index-alist (&optional noerror)
  "Create an index alist for the definitions in the current buffer.
This works by using the hook function `imenu-create-index-function'.
Report an error if the list is empty unless NOERROR is supplied and
non-nil.

See `imenu--index-alist' for the format of the index alist."
  (or (and imenu--index-alist
	   (or (not imenu-auto-rescan)
	       (and imenu-auto-rescan
		    (> (buffer-size)  imenu-auto-rescan-maxout))))
      ;; Get the index; truncate if necessary.
      (progn
	(setq imenu--index-alist
	      (save-excursion
		(save-restriction
		  (widen)
		  (funcall imenu-create-index-function))))
	(imenu--truncate-items imenu--index-alist)))
  (or imenu--index-alist noerror
      (imenu-unavailable-error
       "No items suitable for an index found in this buffer"))
  (or imenu--index-alist
      (setq imenu--index-alist (list nil)))
  (if imenu-auto-rescan
      imenu--index-alist
    ;; Add a rescan option to the index.
    (cons imenu--rescan-item imenu--index-alist)))

(defvar imenu--cleanup-seen nil)

(defun imenu--cleanup (&optional alist)
  "Find all markers in ALIST and make them point nowhere.
If ALIST is nil (the normal case), use `imenu--index-alist'.
Non-nil arguments are in recursive calls."
  ;; If alist is provided use that list.
  ;; If not, empty the table of lists already seen
  ;; and use imenu--index-alist.
  (if alist
      (setq imenu--cleanup-seen (cons alist imenu--cleanup-seen))
    (setq alist imenu--index-alist imenu--cleanup-seen (list alist)))

  (when alist
    (dolist (item alist)
      (cond
       ((markerp (cdr item)) (set-marker (cdr item) nil))
       ;; Don't process one alist twice.
       ((memq (cdr item) imenu--cleanup-seen))
       ((imenu--subalist-p item) (imenu--cleanup (cdr item)))))
    t))

(defun imenu--create-keymap (title alist &optional cmd)
  `(keymap ,title
           ,@(mapcar
              (if imenu-allow-duplicate-menu-items
                  (lambda (item)
                    `(,(car item)
                      ,(car item)
                      ,@(cond
                         ((imenu--subalist-p item)
                          (imenu--create-keymap (car item) (cdr item) cmd))
                         (t
                          (lambda () (interactive)
                            (if cmd (funcall cmd item) item))))))
                (lambda (item)
                  `(,(intern (car item))
                    ,(car item)
                    ,@(cond
                       ((imenu--subalist-p item)
                        (imenu--create-keymap (car item) (cdr item) cmd))
                       (t
                        (lambda () (interactive)
                          (if cmd (funcall cmd item) item)))))))
              (remq nil alist))))

(defun imenu--in-alist (str alist)
  "Check whether the string STR is contained in multi-level ALIST."
  (let (elt head tail res)
    (setq res nil)
    (while alist
      (setq elt (car alist)
	    tail (cdr elt)
	    alist (cdr alist)
	    head (car elt))
      ;; A nested ALIST element looks like
      ;;   (INDEX-NAME (INDEX-NAME . INDEX-POSITION) ...)
      ;; while a bottom-level element looks like
      ;;   (INDEX-NAME . INDEX-POSITION)
      ;; or
      ;;   (INDEX-NAME INDEX-POSITION FUNCTION ARGUMENTS...)
      ;; We are only interested in the bottom-level elements, so we need to
      ;; recurse if TAIL is a nested ALIST.
      (cond ((imenu--subalist-p elt)
	     (if (setq res (imenu--in-alist str tail))
		 (setq alist nil)))
	    ((if imenu-name-lookup-function
                 (funcall imenu-name-lookup-function str head)
               (string= str head))
	     (setq alist nil res elt))))
    res))

;;;###autoload
(defvar-local imenu-syntax-alist nil
  "Alist of syntax table modifiers to use while in `imenu--generic-function'.

The car of the assocs may be either a character or a string and the
cdr is a syntax description appropriate for `modify-syntax-entry'.  For
a string, all the characters in the string get the specified syntax.

This is typically used to give word syntax to characters which
normally have symbol syntax to simplify `imenu-expression'
and speed-up matching.")

(defun imenu-default-create-index-function ()
  "Default function to create an index alist of the current buffer.

The most general method is to move point to end of buffer, then repeatedly call
`imenu-prev-index-position-function' and `imenu-extract-index-name-function'.
All the results returned by the latter are gathered into an index alist.
This method is used if those two variables are non-nil.

The alternate method, which is the one most often used, is to call
`imenu--generic-function' with `imenu-generic-expression' as argument."
  ;; These should really be done by setting imenu-create-index-function
  ;; in these major modes.  But save that change for later.
  (cond ((and imenu-prev-index-position-function
	      imenu-extract-index-name-function)
	 (let ((index-alist '()) (pos (point-max))
               (start (float-time))
	       name)
	   (goto-char pos)
	   ;; Search for the function
	   (while (and (funcall imenu-prev-index-position-function)
                       ;; Don't use an excessive amount of time.
                       (< (- (float-time) start) imenu-max-index-time))
             (unless (< (point) pos)
               (error "Infinite loop at %s:%d: imenu-prev-index-position-function does not move point" (buffer-name) pos))
             (setq pos (point))
	     (save-excursion
	       (setq name (funcall imenu-extract-index-name-function)))
	     (and (stringp name)
 		  ;; [ydi] Updated for imenu-use-markers.
		  (push (cons name
                              (if imenu-use-markers
                                  (copy-marker (point) t)
                                (point)))
			index-alist)))
	   index-alist))
	;; Use generic expression if possible.
	((and imenu-generic-expression)
	 (imenu--generic-function imenu-generic-expression))
	(t
         (imenu-unavailable-error "This buffer cannot use `imenu-default-create-index-function'"))))

;;;
;;; Generic index gathering function.
;;;

;;;###autoload
(defvar-local imenu-case-fold-search t
  "Defines whether `imenu--generic-function' should fold case when matching.

This variable should be set (only) by initialization code
for modes which use `imenu--generic-function'.  If it is not set, but
`font-lock-defaults' is set, then font-lock's setting is used.")

;; This function can be called with quitting disabled,
;; so it needs to be careful never to loop!
(defun imenu--generic-function (patterns)
  "Return an index alist of the current buffer based on PATTERNS.
PATTERNS should be an alist of the same form as `imenu-generic-expression'.

If `imenu-generic-skip-comments-and-strings' is non-nil, this ignores
text inside comments and strings.

If `imenu-case-fold-search' is non-nil, this ignores case.

The return value is an alist of the form
 (INDEX-NAME . INDEX-POSITION)
or
 (INDEX-NAME INDEX-POSITION FUNCTION ARGUMENTS...)
The return value may also consist of nested index alists like:
 (INDEX-NAME . INDEX-ALIST)
depending on PATTERNS."
  (let ((index-alist (list 'dummy))
        (case-fold-search (if (or (local-variable-p 'imenu-case-fold-search)
				  (not (local-variable-p 'font-lock-defaults)))
			      imenu-case-fold-search
			    (nth 2 font-lock-defaults)))
        (start-time (float-time))
        (old-table (syntax-table))
        (table (copy-syntax-table (syntax-table)))
        (slist imenu-syntax-alist))
    ;; Modify the syntax table used while matching regexps.
    (dolist (syn slist)
      ;; The character(s) to modify may be a single char or a string.
      (if (numberp (car syn))
	  (modify-syntax-entry (car syn) (cdr syn) table)
        (mapc (lambda (c)
                (modify-syntax-entry c (cdr syn) table))
              (car syn))))
    (goto-char (point-max))
    (unwind-protect			; For syntax table.
	(save-match-data
	  (set-syntax-table table)

	  ;; Map over the elements of imenu-generic-expression
	  ;; (typically functions, variables ...).
	  (dolist (pat patterns)
	    (let ((menu-title (car pat))
		  (regexp (nth 1 pat))
		  (index (nth 2 pat))
		  (function (nth 3 pat))
		  (rest (nthcdr 4 pat))
		  start beg)
	      ;; Go backwards for convenience of adding items in order.
	      (goto-char (point-max))
	      (while (and (if (functionp regexp)
			      (funcall regexp)
			    (and
			     (re-search-backward regexp nil t)
			     ;; Do not count invisible definitions.
			     (let ((invis (invisible-p (point))))
			       (or (not invis)
				   (progn
				     (while (and invis
						 (not (bobp)))
				       (setq invis (not (re-search-backward
							 regexp nil 'move))))
				     (not invis))))))
			  ;; Exit the loop if we get an empty match,
			  ;; because it means a bad regexp was specified.
			  (not (= (match-beginning 0) (match-end 0)))
                          ;; Don't take an excessive amount of time.
                          (or (< (- (float-time) start-time)
                                 imenu-max-index-time)
                              (progn
                                (message "`imenu-max-index-time' exceeded")
                                nil)))
		(setq start (point))
		;; Record the start of the line in which the match starts.
		;; That's the official position of this definition.
		(goto-char (match-beginning index))
		(beginning-of-line)
		(setq beg (point))
		;; Add this sort of submenu only when we've found an
		;; item for it, avoiding empty, duff menus.
		(unless (assoc menu-title index-alist)
		  (push (list menu-title) index-alist))
		(if imenu-use-markers
		    (setq beg (copy-marker beg t)))
		(let ((item
		       (if function
			   (nconc (list (match-string-no-properties index)
					beg function)
				  rest)
			 (cons (match-string-no-properties index)
			       beg)))
		      ;; This is the desired submenu,
		      ;; starting with its title (or nil).
		      (menu (assoc menu-title index-alist)))
		  ;; Insert the item unless it is already present.
		  (unless (or (member item (cdr menu))
                              (and imenu-generic-skip-comments-and-strings
                                   (save-excursion
                                     (goto-char start) (nth 8 (syntax-ppss)))))
		    (setcdr menu
			    (cons item (cdr menu)))))
		;; Go to the start of the match, to make sure we
		;; keep making progress backwards.
		(goto-char start)))))
      (set-syntax-table old-table))
    ;; Sort each submenu by position.
    ;; This is in case one submenu gets items from two different regexps.
    (dolist (item index-alist)
      (when (listp item)
	(setcdr item (sort (cdr item) 'imenu--sort-by-position))))
    ;; Remove any empty menus.  That can happen because of skipping
    ;; things inside comments or strings.
    (setq index-alist (cl-delete-if
                       (lambda (it) (and (consp it) (null (cdr it))))
                       index-alist))
    (let ((main-element (assq nil index-alist)))
      (nconc (delq main-element (delq 'dummy index-alist))
             (cdr main-element)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; The main functions for this package!
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; See also info-lookup-find-item
(defun imenu-find-default (guess completions)
  "Fuzzily find an item based on GUESS inside the alist COMPLETIONS."
  (catch 'found
    (let ((case-fold-search t))
      (if (assoc guess completions) guess
	(dolist (re (list (concat "\\`" (regexp-quote guess) "\\'")
			  (concat "\\`" (regexp-quote guess))
			  (concat (regexp-quote guess) "\\'")
			  (regexp-quote guess)))
	  (dolist (x completions)
	    (if (string-match re (car x)) (throw 'found (car x)))))))))

(defun imenu--completion-buffer (index-alist &optional prompt)
  "Let the user select from INDEX-ALIST in a completion buffer with PROMPT.

Return one of the entries in index-alist or nil."
  ;; Create a list for this buffer only when needed.
  (let ((name (thing-at-point 'symbol))
	choice
	(prepared-index-alist
	 (if (not imenu-space-replacement) index-alist
	   (mapcar
	    (lambda (item)
	      (cons (subst-char-in-string ?\s (aref imenu-space-replacement 0)
					  (car item))
		    (cdr item)))
	    index-alist))))
    (when (stringp name)
      (setq name (or (imenu-find-default name prepared-index-alist) name)))
    (unless prompt
      (setq prompt (format-prompt
                    "Index item"
	            (and name
                         (imenu--in-alist name prepared-index-alist)
                         ;; Default to `name' if it's in the alist.
                         name))))
    (minibuffer-with-setup-hook
        (lambda () (setq-local minibuffer-allow-text-properties t))
      (setq name (completing-read
                  prompt
                  (completion-table-with-metadata
		   prepared-index-alist
                   `((category . imenu)
                     (eager-display . ,(not imenu-eager-completion-buffer))
                     ,@(when (eq imenu-flatten 'annotation)
                         `((annotation-function
                            . ,(lambda (s) (get-text-property
                                            0 'imenu-section s)))))
                     ,@(when (eq imenu-flatten 'group)
                         `((group-function
                            . ,(lambda (s transform)
                                 (if transform s
                                   (get-text-property
                                    0 'imenu-section s))))))))
		  nil t nil 'imenu--history-list name)))

    (when (stringp name)
      (or (get-text-property 0 'imenu-choice name)
	  (progn
	    (setq choice (assoc name prepared-index-alist))
	    (if (imenu--subalist-p choice)
		(imenu--completion-buffer (cdr choice) prompt)
	      choice))))))

(defun imenu--mouse-menu (index-alist event &optional title)
  "Let the user select from a buffer index from a mouse menu.

INDEX-ALIST is the buffer index and EVENT is a mouse event.

Returns t for rescan and otherwise an element or subelement of INDEX-ALIST."
  (setq index-alist (imenu--split-submenus index-alist))
  (let* ((menu (imenu--split-menu index-alist (or title (buffer-name))))
	 (map (imenu--create-keymap (car menu)
                                    (cdr (if (and (null (cddr menu))
                                                  (stringp (caadr menu))
                                                  (consp (cdadr menu)))
                                             (cadr menu)
                                           menu)))))
    (popup-menu map event)))

(defun imenu--flatten-index-alist (index-alist &optional concat-names prefix)
  ;; Takes a nested INDEX-ALIST and returns a flat index alist.
  ;; If optional CONCAT-NAMES is non-nil, then a nested index has its
  ;; name and a space concatenated to the names of the children.
  ;; Third argument PREFIX is for internal use only.
  (mapcan
   (lambda (item)
     (let* ((name (car item))
	    (pos (cdr item))
	    (new-prefix (and concat-names
			     (if prefix
				 (concat prefix imenu-level-separator name)
			       name))))
       (cond
	((not (imenu--subalist-p item))
	 (list (cons (pcase imenu-flatten
                       ('annotation
                        (if prefix
                            (propertize name
                                        'imenu-section (format " (%s)" prefix)
                                        'imenu-choice item)
                          (propertize new-prefix 'imenu-choice item)))
                       ('group (propertize name
                                           'imenu-section (or prefix "*")
                                           'imenu-choice item))
                       (_ new-prefix))
		     pos)))
	(t
	 (imenu--flatten-index-alist pos concat-names new-prefix)))))
   index-alist))

(defun imenu-choose-buffer-index (&optional prompt alist)
  "Let the user select from a buffer index and return the chosen index.

If the user originally activated this function with the mouse, a mouse
menu is used.  Otherwise a completion buffer is used and the user is
prompted with PROMPT.

If you call this function with index alist ALIST, then it lets the user
select from ALIST.

With no index alist ALIST, it calls `imenu--make-index-alist' to
create the index alist.

If `imenu-use-popup-menu' is nil, then the completion buffer
is always used, no matter if the mouse was used or not.

The returned value is of the form (INDEX-NAME . INDEX-POSITION)."
  (let (index-alist
	(mouse-triggered (listp last-nonmenu-event))
	(result t))
    ;; If selected by mouse, see to that the window where the mouse is
    ;; really is selected.
    (and mouse-triggered
	 (not (equal last-nonmenu-event '(menu-bar)))
	 (let ((window (posn-window (event-start last-nonmenu-event))))
	   (or (framep window) (null window) (select-window window))))
    ;; Create a list for this buffer only when needed.
    (while (eq result t)
      (setq index-alist (if alist alist (imenu--make-index-alist)))
      (when imenu-flatten
        (setq index-alist (imenu--flatten-index-alist index-alist t)))
      (setq result
	    (if (and imenu-use-popup-menu
		     (or (eq imenu-use-popup-menu t) mouse-triggered))
		(imenu--mouse-menu index-alist last-nonmenu-event)
	      (imenu--completion-buffer index-alist prompt)))
      (and (equal result imenu--rescan-item)
	   (imenu--cleanup)
	   (setq result t imenu--index-alist nil)))
    result))

(defvar-local imenu--menubar-keymap nil)

;;;###autoload
(defun imenu-add-to-menubar (name)
  "Add an `imenu' entry to the menu bar for the current buffer.
NAME is a string used to name the menu bar item.
See the command `imenu' for more information."
  (interactive "sImenu menu item name: ")
  (if (or (and imenu-prev-index-position-function
	       imenu-extract-index-name-function)
	  imenu-generic-expression
	  (not (eq imenu-create-index-function
		   'imenu-default-create-index-function)))
      (unless (and (current-local-map)
                   (keymapp (lookup-key (current-local-map) [menu-bar index])))
	(let ((newmap (make-sparse-keymap)))
	  (set-keymap-parent newmap (current-local-map))
	  (setq imenu--last-menubar-index-alist nil)
          (setq imenu--menubar-keymap (make-sparse-keymap "Imenu"))
	  (define-key newmap [menu-bar index]
	    `(menu-item ,name ,imenu--menubar-keymap))
	  (use-local-map newmap)
	  (add-hook 'menu-bar-update-hook 'imenu-update-menubar)))
    (imenu-unavailable-error "The mode `%s' does not support Imenu"
                             (format-mode-line mode-name))))

;;;###autoload
(defun imenu-add-menubar-index ()
  "Add an Imenu \"Index\" entry on the menu bar for the current buffer.

A trivial interface to `imenu-add-to-menubar' suitable for use in a hook."
  (interactive)
  (imenu-add-to-menubar "Index"))

(defvar-local imenu-menubar-modified-tick 0
  "Value of (buffer-chars-modified-tick) when `imenu-update-menubar' was called.")

(defun imenu-update-menubar ()
  (when (and (current-local-map)
             imenu--menubar-keymap
	     (/= (buffer-chars-modified-tick) imenu-menubar-modified-tick))
    (setq imenu-menubar-modified-tick (buffer-chars-modified-tick))
    (let ((index-alist (imenu--make-index-alist t)))
      ;; Don't bother updating if the index-alist has not changed
      ;; since the last time we did it.
      (unless (equal index-alist imenu--last-menubar-index-alist)
        (setq imenu--last-menubar-index-alist index-alist)
        (setq index-alist (imenu--split-submenus index-alist))
	(let* ((menu (imenu--split-menu index-alist
                                        (buffer-name)))
               (menu1 (imenu--create-keymap
                       (car menu)
                       (cdr (if (and (null (cddr menu))
                                     (stringp (caadr menu))
                                     (consp (cdadr menu)))
                                (cadr menu)
                              menu))
                       'imenu--menubar-select)))
	  (setcdr imenu--menubar-keymap (cdr menu1)))))))

(defun imenu--menubar-select (item)
  "Use Imenu to select the function or variable named in this menu ITEM."
  (if (equal item imenu--rescan-item)
      (progn
	(imenu--cleanup)
	;; Make sure imenu-update-menubar redoes everything.
	(setq imenu-menubar-modified-tick -1)
	(setq imenu--index-alist nil)
	(setq imenu--last-menubar-index-alist nil)
	(imenu-update-menubar)
	t)
    (imenu item)
    nil))

(defun imenu-default-goto-function (_name position &rest _rest)
  "Move to the given position.

NAME is ignored.  POSITION is where to move.  REST is also ignored.
The ignored args just make this function have the same interface as a
function placed in a special index-item."
  (if (or (< position (point-min))
	  (> position (point-max)))
      ;; Widen if outside narrowing.
      (widen))
  (goto-char position))

;;;###autoload
(defun imenu (index-item)
  "Jump to a place in the buffer chosen using a buffer menu or mouse menu.
INDEX-ITEM specifies the position.  See `imenu-choose-buffer-index'
for more information."
  (interactive (list (imenu-choose-buffer-index)))
  ;; Convert a string to an alist element.
  (if (stringp index-item)
      (setq index-item (assoc index-item (imenu--make-index-alist))))
  (when index-item
    (pcase index-item
      (`(,name ,pos ,fn . ,args)
       (push-mark nil t)
       (apply fn name pos args)
       (run-hooks 'imenu-after-jump-hook))
      (`(,name . ,pos) (imenu (list name pos imenu-default-goto-function)))
      (_ (error "Unknown imenu item: %S" index-item)))))

(defun imenu-flush-cache ()
  "Flush the current imenu cache.
This forces a full rescan of the buffer to recreate the index alist
next time `imenu' is invoked."
  (imenu--cleanup)
  (setq imenu--index-alist nil))

(provide 'imenu)

;;; imenu.el ends here
