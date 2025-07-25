;;; replace.el --- replace commands for Emacs -*- lexical-binding: t -*-

;; Copyright (C) 1985-1987, 1992, 1994, 1996-1997, 2000-2025 Free
;; Software Foundation, Inc.

;; Maintainer: emacs-devel@gnu.org
;; Package: emacs

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

;; This package supplies the string and regular-expression replace functions
;; documented in the Emacs user's manual.

;;; Code:

(require 'text-mode)
(eval-when-compile (require 'cl-lib))
(eval-when-compile (require 'subr-x))

(defcustom case-replace t
  "Non-nil means `query-replace' should preserve case in replacements."
  :type 'boolean
  :group 'matching)

(defcustom replace-char-fold nil
  "Non-nil means replacement commands should do character folding in matches.
This means, for instance, that \\=' will match a large variety of
Unicode quotes.
This variable affects `query-replace' and `replace-string', but not
`replace-regexp'."
  :type 'boolean
  :group 'matching
  :version "25.1")

(defcustom replace-lax-whitespace nil
  "Non-nil means `query-replace' matches a sequence of whitespace chars.
When you enter a space or spaces in the strings to be replaced,
it will match any sequence matched by the regexp `search-whitespace-regexp'."
  :type 'boolean
  :group 'matching
  :version "24.3")

(defcustom replace-regexp-lax-whitespace nil
  "Non-nil means `query-replace-regexp' matches a sequence of whitespace chars.
When you enter a space or spaces in the regexps to be replaced,
it will match any sequence matched by the regexp `search-whitespace-regexp'."
  :type 'boolean
  :group 'matching
  :version "24.3")

(defvar query-replace-history nil
  "Default history list for `query-replace' commands.
See `query-replace-from-history-variable' and
`query-replace-to-history-variable'.")

(defvar query-replace-defaults nil
  "Default values of FROM-STRING and TO-STRING for `query-replace'.
This is a list of cons cells (FROM-STRING . TO-STRING), or nil
if there are no default values.")

(defcustom query-replace-from-to-separator " → "
  "String that separates FROM and TO in the history of replacement pairs.
When nil, the pair will not be added to the history (same behavior
as in Emacs 24.5)."
  :group 'matching
  :type '(choice
          (const :tag "Disabled" nil)
          string)
  :version "25.1")

(defcustom query-replace-from-history-variable 'query-replace-history
  "History list to use for the FROM argument of `query-replace' commands.
The value of this variable should be a symbol; that symbol
is used as a variable to hold a history list for the strings
or patterns to be replaced."
  :group 'matching
  :type 'symbol
  :version "20.3")

(defcustom query-replace-to-history-variable 'query-replace-history
  "History list to use for the TO argument of `query-replace' commands.
The value of this variable should be a symbol; that symbol
is used as a variable to hold a history list for replacement
strings or patterns."
  :group 'matching
  :type 'symbol
  :version "20.3")

(defcustom query-replace-skip-read-only nil
  "Non-nil means `query-replace' and friends ignore read-only matches."
  :type 'boolean
  :group 'matching
  :version "22.1")

(defcustom query-replace-show-replacement t
  "Non-nil means show substituted replacement text in the minibuffer.
This variable affects only `query-replace-regexp'."
  :type 'boolean
  :group 'matching
  :version "23.1")

(defcustom query-replace-highlight t
  "Non-nil means to highlight matches during query replacement."
  :type 'boolean
  :group 'matching)

(defcustom query-replace-highlight-submatches t
  "Whether to highlight regexp subexpressions during query replacement.
The faces used to do the highlights are named `isearch-group-1',
`isearch-group-2', etc.  (By default, only these 2 are defined.)
When there are more matches than faces, then faces are reused from the
beginning, in a cyclical manner, so the `isearch-group-1' face is
isreused for the third match.  If you want to use more distinctive colors,
you can define more of these faces using the same numbering scheme."
  :type 'boolean
  :group 'matching
  :version "28.1")

(defcustom query-replace-lazy-highlight t
  "Controls the lazy-highlighting during query replacements.
When non-nil, all text in the buffer matching the current match
is highlighted lazily using isearch lazy highlighting (see
`lazy-highlight-initial-delay' and `lazy-highlight-interval')."
  :type 'boolean
  :group 'lazy-highlight
  :group 'matching
  :version "22.1")

(defface query-replace
  '((t (:inherit isearch)))
  "Face for highlighting query replacement matches."
  :group 'matching
  :version "22.1")

(defvar replace-count 0
  "Number of replacements done so far.
See `replace-regexp'.")

(defun query-replace-descr (string)
  (setq string (copy-sequence string))
  (dotimes (i (length string))
    (let ((c (aref string i)))
      (cond
       ((< c ?\s) (add-text-properties
                   i (1+ i)
                   `(display ,(propertize (format "^%c" (+ c 64)) 'face 'escape-glyph))
                   string))
       ((= c ?\^?) (add-text-properties
	            i (1+ i)
                    `(display ,(propertize "^?" 'face 'escape-glyph))
                    string)))))
  string)

(defun query-replace--split-string (string)
  "Split string STRING at a substring with property `separator'."
  (let* ((length (length string))
         (split-pos (text-property-any 0 length 'separator t string)))
    (if (not split-pos)
        string
      (cons (substring string 0 split-pos)
            (substring-no-properties
             string (or (text-property-not-all
                         (1+ split-pos) length 'separator t string)
                        length)
             length)))))

(defvar query-replace-read-from-default nil
  "Function to get default non-regexp value for `query-replace-read-from'.")

(defvar query-replace-read-from-regexp-default nil
  "Function to get default regexp value for `query-replace-read-from'.")

(defun query-replace-read-from-suggestions ()
  "Return a list of standard suggestions for `query-replace-read-from'.
By default, the list includes the active region, the identifier
(a.k.a. \"tag\") at point (see Info node `(emacs) Identifier Search'),
the last isearch string, and the last replacement regexp.
`query-replace-read-from' appends the list returned
by this function to the end of values available via
\\<minibuffer-local-map>\\[next-history-element]."
  (delq nil (list (when (use-region-p)
                    (buffer-substring-no-properties
                     (region-beginning) (region-end)))
                  (find-tag-default)
                  (car search-ring)
                  (car (symbol-value query-replace-from-history-variable)))))

(defun query-replace-read-from (prompt regexp-flag)
  "Query and return the `from' argument of a `query-replace' operation.
Prompt with PROMPT.  REGEXP-FLAG non-nil means the response should be a regexp.
The return value can also be a pair (FROM . TO) indicating that the user
wants to replace FROM with TO."
  (let* ((history-add-new-input nil)
         (separator-string
          (when query-replace-from-to-separator
            ;; Check if the first non-whitespace char is displayable
            (if (char-displayable-p
                 (string-to-char (string-replace
                                  " " "" query-replace-from-to-separator)))
                query-replace-from-to-separator
              " -> ")))
         (separator
          (when separator-string
            (propertize separator-string
                        'display separator-string
                        'face 'minibuffer-prompt
                        'separator t)))
         (minibuffer-history
          (append
           (when separator
             (mapcar (lambda (from-to)
                       (concat (query-replace-descr (car from-to))
                               separator
                               (query-replace-descr (cdr from-to))))
                     query-replace-defaults))
           (symbol-value query-replace-from-history-variable)))
         (minibuffer-allow-text-properties t) ; separator uses text-properties
         (default (when (and query-replace-read-from-default (not regexp-flag))
                    (funcall query-replace-read-from-default)))
         (prompt
          (cond ((and query-replace-read-from-regexp-default regexp-flag) prompt)
                (default (format-prompt prompt default))
                ((and query-replace-defaults separator)
                 (format-prompt prompt (car minibuffer-history)))
                (query-replace-defaults
                 (format-prompt
                  prompt (format "%s -> %s"
                                 (query-replace-descr
                                  (caar query-replace-defaults))
                                 (query-replace-descr
                                  (cdar query-replace-defaults)))))
                (t (format-prompt prompt nil))))
         (from
          ;; The save-excursion here is in case the user marks and copies
          ;; a region in order to specify the minibuffer input.
          ;; That should not clobber the region for the query-replace itself.
          (save-excursion
            (minibuffer-with-setup-hook
                (lambda ()
                  (setq-local text-property-default-nonsticky
                              (append '((separator . t) (face . t))
                                      text-property-default-nonsticky)))
              (if regexp-flag
                  (read-regexp
                   (if query-replace-read-from-regexp-default
                       (string-remove-suffix ": " prompt)
                     prompt)
                   query-replace-read-from-regexp-default
                   'minibuffer-history)
                (read-from-minibuffer
                 prompt nil nil nil nil
                 (if default
                     (delete-dups
                      (cons default (query-replace-read-from-suggestions)))
                   (query-replace-read-from-suggestions))
                 t)))))
         (to))
    (if (and (zerop (length from)) query-replace-defaults (not default))
        (cons (caar query-replace-defaults)
              (query-replace-compile-replacement
               (cdar query-replace-defaults) regexp-flag))
      (setq from (or (and (zerop (length from)) default)
                     (query-replace--split-string from)))
      (when (consp from) (setq to (cdr from) from (car from)))
      (add-to-history query-replace-from-history-variable from nil t)
      ;; Warn if user types \n or \t, but don't reject the input.
      (and regexp-flag
           (string-match "\\(\\`\\|[^\\]\\)\\(\\\\\\\\\\)*\\(\\\\[nt]\\)" from)
           (let ((match (match-string 3 from)))
             (cond
              ((string= match "\\n")
               (message "Note: `\\n' here doesn't match a newline; to do that, type C-q C-j instead"))
              ((string= match "\\t")
               (message "Note: `\\t' here doesn't match a tab; to do that, just type TAB")))
             (sit-for 2)))
      (if (not to)
          from
        (add-to-history query-replace-to-history-variable to nil t)
        (add-to-history 'query-replace-defaults (cons from to) nil t)
        (cons from (query-replace-compile-replacement to regexp-flag))))))

(defun query-replace-compile-replacement (to regexp-flag)
  "Maybe convert a regexp replacement TO to Lisp.
REGEXP-FLAG non-nil means TO is a regexp.
Returns a list suitable for `perform-replace' if necessary,
the original string if not."
  (if (and regexp-flag
	   (string-match "\\(\\`\\|[^\\]\\)\\(\\\\\\\\\\)*\\\\[,#]" to))
      (let (pos list char)
	(while
	    (progn
	      (setq pos (match-end 0))
	      (push (substring to 0 (- pos 2)) list)
	      (setq char (aref to (1- pos))
		    to (substring to pos))
	      (cond ((eq char ?\#)
		     (push '(number-to-string replace-count) list))
		    ((eq char ?\,)
		     (setq pos (read-from-string to))
		     (push `(replace-quote ,(car pos)) list)
		     (let ((end
			    ;; Swallow a space after a symbol
			    ;; if there is a space.
			    (if (and (or (symbolp (car pos))
					 ;; Swallow a space after 'foo
					 ;; but not after (quote foo).
					 (and (eq (car-safe (car pos)) 'quote)
					      (not (= ?\( (aref to 0)))))
				     (eq (string-search " " to (cdr pos))
					 (cdr pos)))
				(1+ (cdr pos))
			      (cdr pos))))
		       (setq to (substring to end)))))
	      (string-match "\\(\\`\\|[^\\]\\)\\(\\\\\\\\\\)*\\\\[,#]" to)))
	(setq to (nreverse (delete "" (cons to list))))
	(replace-match-string-symbols to)
	(cons #'replace-eval-replacement
	      (if (cdr to)
		  (cons 'concat to)
		(car to))))
    to))


(defun query-replace-read-to (from prompt regexp-flag)
  "Query and return the `to' argument of a `query-replace' operation.
Prompt with PROMPT.  REGEXP-FLAG non-nil means the response
should a regexp."
  (query-replace-compile-replacement
   (save-excursion
     (let* ((history-add-new-input nil)
	    (to (read-from-minibuffer
		 (format "%s %s with: " prompt (query-replace-descr from))
		 nil nil nil
		 query-replace-to-history-variable from t)))
       (add-to-history query-replace-to-history-variable to nil t)
       (add-to-history 'query-replace-defaults (cons from to) nil t)
       to))
   regexp-flag))

(defun query-replace-read-args (prompt regexp-flag &optional noerror no-highlight)
  (unless noerror
    (barf-if-buffer-read-only))
  (save-mark-and-excursion
    (let* ((delimited-flag (and current-prefix-arg
                                (not (eq current-prefix-arg '-))))
           (from (minibuffer-with-setup-hook
                     (minibuffer-lazy-highlight-setup
                      :case-fold case-fold-search
                      :filter (when (use-region-p)
                                (replace--region-filter
                                 (funcall region-extract-function 'bounds)))
                      :highlight (and query-replace-lazy-highlight (not no-highlight))
                      :regexp regexp-flag
                      :regexp-function (or replace-regexp-function
                                           delimited-flag
                                           (and replace-char-fold
	                                        (not regexp-flag)
	                                        #'char-fold-to-regexp))
                      :transform (lambda (string)
                                   (let* ((split (query-replace--split-string string))
                                          (from-string (if (consp split) (car split) split)))
                                     (when (and case-fold-search search-upper-case)
	                               (setq isearch-case-fold-search
                                             (isearch-no-upper-case-p from-string regexp-flag)))
                                     from-string)))
                   (query-replace-read-from prompt regexp-flag)))
           (to (if (consp from) (prog1 (cdr from) (setq from (car from)))
                 (query-replace-read-to from prompt regexp-flag))))
      (list from to
            (or delimited-flag
                (and (plist-member (text-properties-at 0 from) 'isearch-regexp-function)
                     (get-text-property 0 'isearch-regexp-function from)))
            (and current-prefix-arg (eq current-prefix-arg '-))))))

(defun query-replace (from-string to-string &optional delimited start end backward region-noncontiguous-p)
  "Replace some occurrences of FROM-STRING with TO-STRING.
As each match is found, the user must type a character saying
what to do with it.  Type SPC or `y' to replace the match,
DEL or `n' to skip and go to the next match.  For more directions,
type \\[help-command] at that time.

In Transient Mark mode, if the mark is active, operate on the contents
of the region.  Otherwise, operate from point to the end of the buffer's
accessible portion.

In interactive use, the prefix arg (non-nil DELIMITED in
non-interactive use), means replace only matches surrounded by
word boundaries.  A negative prefix arg means replace backward.

Use \\<minibuffer-local-map>\\[next-history-element] \
to pull the last incremental search string to the minibuffer
that reads FROM-STRING, or invoke replacements from
incremental search with a key sequence like \\`C-s C-s M-%'
to use its current search string as the string to replace.

Matching is independent of case if both `case-fold-search'
and `search-upper-case' are non-nil and FROM-STRING has no
uppercase letters; if `search-upper-case' is nil, then
whether matching ignores case depends on `case-fold-search'
regardless of whether there are uppercase letters in FROM-STRING.
Replacement transfers the case pattern of the old text to the
new text, if both `case-fold-search' and `case-replace' are
non-nil and FROM-STRING has no uppercase letters.
\(Transferring the case pattern means that if the old text
matched is all caps, or all of its words are capitalized, then its
replacement is respectively upcased or capitalized.  For more
details about this, see `replace-match'.)

Ignore read-only matches if `query-replace-skip-read-only' is non-nil,
ignore hidden matches if `search-invisible' is nil, and ignore more
matches using `isearch-filter-predicate'.

If `replace-lax-whitespace' is non-nil, a space or spaces in the string
to be replaced will match a sequence of whitespace chars defined by the
regexp in `search-whitespace-regexp'.

If `replace-char-fold' is non-nil, matching uses character folding,
i.e. it ignores diacritics and other differences between equivalent
character strings.

Fourth and fifth arg START and END specify the region to operate on.

Arguments FROM-STRING, TO-STRING, DELIMITED, START, END, BACKWARD, and
REGION-NONCONTIGUOUS-P are passed to `perform-replace' (which see).
\(TO-STRING is passed to `perform-replace' as REPLACEMENTS and
DELIMITED is passed as DELIMITED-FLAG.)

To customize possible responses, change the bindings in `query-replace-map'."
  (declare (interactive-args
	    (start (use-region-beginning))
	    (end (use-region-end))
	    (region-noncontiguous-p (use-region-noncontiguous-p))))
  (interactive
   (let ((common
	  (query-replace-read-args
	   (concat "Query replace"
		   (if current-prefix-arg
		       (if (eq current-prefix-arg '-) " backward" " word")
		     "")
		   (if (use-region-p) " in region" ""))
	   nil)))
     (list (nth 0 common) (nth 1 common) (nth 2 common)
	   ;; These are done separately here
	   ;; so that command-history will record these expressions
	   ;; rather than the values they had this time.
	   (use-region-beginning) (use-region-end)
	   (nth 3 common)
	   (use-region-noncontiguous-p))))
  (perform-replace from-string to-string t nil delimited nil nil start end backward region-noncontiguous-p))

(define-key esc-map "%" 'query-replace)

(defun query-replace-regexp (regexp to-string &optional delimited start end backward region-noncontiguous-p)
  "Replace some things after point matching REGEXP with TO-STRING.
As each match is found, the user must type a character saying
what to do with it.  Type \\`SPC' or \\`y' to replace the match,
\\`DEL' or \\`n' to skip and go to the next match.  For more directions,
type \\[help-command] at that time.

In Transient Mark mode, if the mark is active, operate on the contents
of the region.  Otherwise, operate from point to the end of the buffer's
accessible portion.

When invoked interactively, matching a newline with `\\n' will not work;
use \\`C-q C-j' instead.  To match a tab character (`\\t'), just press \\`TAB'.

Use \\<minibuffer-local-map>\\[next-history-element] \
to pull the last incremental search regexp to the minibuffer
that reads REGEXP, or invoke replacements from
incremental search with a key sequence like \\`C-M-s C-M-s C-M-%'
to use its current search regexp as the regexp to replace.

Matching is independent of case if both `case-fold-search'
and `search-upper-case' are non-nil and REGEXP has no uppercase
letters; if `search-upper-case' is nil, then whether matching
ignores case depends on `case-fold-search' regardless of whether
there are uppercase letters in REGEXP.
Replacement transfers the case pattern of the old text to the new
text, if both `case-fold-search' and `case-replace' are non-nil
and REGEXP has no uppercase letters.  (Transferring the case pattern
means that if the old text matched is all caps, or all of its words
are capitalized, then its replacement is respectively upcased or
capitalized.  For more details about this, see `replace-match'.)

Ignore read-only matches if `query-replace-skip-read-only' is non-nil,
ignore hidden matches if `search-invisible' is nil, and ignore more
matches using `isearch-filter-predicate'.

If `replace-regexp-lax-whitespace' is non-nil, a space or spaces in the regexp
to be replaced will match a sequence of whitespace chars defined by the
regexp in `search-whitespace-regexp'.

This function is not affected by `replace-char-fold'.

Third arg DELIMITED (prefix arg if interactive), if non-nil, means replace
only matches surrounded by word boundaries.  A negative prefix arg means
replace backward.

Fourth and fifth arg START and END specify the region to operate on.

In TO-STRING, `\\&' stands for whatever matched the whole of REGEXP,
and `\\=\\N' (where N is a digit) stands for whatever matched
the Nth `\\(...\\)' (1-based) in REGEXP.  The `\\(...\\)' groups are
counted from 1.
`\\?' lets you edit the replacement text in the minibuffer
at the given position for each replacement.

In interactive calls, the replacement text can contain `\\,'
followed by a Lisp expression.  Each
replacement evaluates that expression to compute the replacement
string.  Inside of that expression, `\\&' is a string denoting the
whole match as a string, `\\N' for a partial match, `\\#&' and `\\#N'
for the whole or a partial match converted to a number with
`string-to-number', and `\\#' itself for the number of replacements
done so far (starting with zero).

If the replacement expression is a symbol, write a space after it
to terminate it.  One space there, if any, will be discarded.

When using those Lisp features interactively in the replacement
text, TO-STRING is actually made a list instead of a string.
Use \\[repeat-complex-command] after this command for details.

Arguments REGEXP, TO-STRING, DELIMITED, START, END, BACKWARD, and
REGION-NONCONTIGUOUS-P are passed to `perform-replace' (which see).
\(REGEXP is passed to `perform-replace' as FROM-STRING,
TO-STRING is passed as REPLACEMENTS, and DELIMITED is passed
as DELIMITED-FLAG.)"
  (declare (interactive-args
	    (start (use-region-beginning))
	    (end (use-region-end))
	    (region-noncontiguous-p (use-region-noncontiguous-p))))
  (interactive
   (let ((common
	  (query-replace-read-args
	   (concat "Query replace"
		   (if current-prefix-arg
		       (if (eq current-prefix-arg '-) " backward" " word")
		     "")
		   " regexp"
		   (if (use-region-p) " in region" ""))
	   t)))
     (list (nth 0 common) (nth 1 common) (nth 2 common)
	   ;; These are done separately here
	   ;; so that command-history will record these expressions
	   ;; rather than the values they had this time.
	   (use-region-beginning) (use-region-end)
	   (nth 3 common)
	   (use-region-noncontiguous-p))))
  (perform-replace regexp to-string t t delimited nil nil start end backward region-noncontiguous-p))

(define-key esc-map [?\C-%] 'query-replace-regexp)

(defun map-query-replace-regexp (regexp to-strings &optional n start end region-noncontiguous-p)
  "Replace some matches for REGEXP with various strings, in rotation.
The second argument TO-STRINGS contains the replacement strings, separated
by spaces.  This command works like `query-replace-regexp' except that
each successive replacement uses the next successive replacement string,
wrapping around from the last such string to the first.

In Transient Mark mode, if the mark is active, operate on the contents
of the region.  Otherwise, operate from point to the end of the buffer's
accessible portion.

Non-interactively, TO-STRINGS may be a list of replacement strings.

Interactively, reads the regexp using `read-regexp'.
Use \\<minibuffer-local-map>\\[next-history-element] \
to pull the last incremental search regexp to the minibuffer
that reads REGEXP.

As each match is found, the user must type a character saying
what to do with it.  Type SPC or `y' to replace the match,
DEL or `n' to skip and go to the next match.  For more directions,
type \\[help-command] at that time.

A prefix argument N says to use each replacement string N times
before rotating to the next.
Fourth and fifth arg START and END specify the region to operate on.

Arguments REGEXP, START, END, and REGION-NONCONTIGUOUS-P are passed to
`perform-replace' (which see)."
  (declare (interactive-args
	    (start (use-region-beginning))
	    (end (use-region-end))
	    (region-noncontiguous-p (use-region-noncontiguous-p))))
  (interactive
   (let* ((from (read-regexp "Map query replace (regexp): " nil
			     query-replace-from-history-variable))
	  (to (read-from-minibuffer
	       (format "Query replace %s with (space-separated strings): "
		       (query-replace-descr from))
	       nil nil nil
	       query-replace-to-history-variable from t)))
     (list from to
	   (and current-prefix-arg
		(prefix-numeric-value current-prefix-arg))
	   (use-region-beginning) (use-region-end)
	   (use-region-noncontiguous-p))))
  (let (replacements)
    (if (listp to-strings)
	(setq replacements to-strings)
      (while (/= (length to-strings) 0)
	(if (string-search " " to-strings)
	    (setq replacements
		  (append replacements
			  (list (substring to-strings 0
					   (string-search " " to-strings))))
		  to-strings (substring to-strings
				       (1+ (string-search " " to-strings))))
	  (setq replacements (append replacements (list to-strings))
		to-strings ""))))
    (perform-replace regexp replacements t t nil n nil start end nil region-noncontiguous-p)))

(defun replace-string (from-string to-string &optional delimited start end backward region-noncontiguous-p)
  "Replace occurrences of FROM-STRING with TO-STRING.
Preserve case in each match if `case-replace' and `case-fold-search'
are non-nil and FROM-STRING has no uppercase letters.
\(Preserving case means that if the string matched is all caps, or capitalized,
then its replacement is upcased or capitalized.)

Ignore read-only matches if `query-replace-skip-read-only' is non-nil,
ignore hidden matches if `search-invisible' is nil, and ignore more
matches using `isearch-filter-predicate'.

If `replace-lax-whitespace' is non-nil, a space or spaces in the string
to be replaced will match a sequence of whitespace chars defined by the
regexp in `search-whitespace-regexp'.

If `replace-char-fold' is non-nil, matching uses character folding,
i.e. it ignores diacritics and other differences between equivalent
character strings.

Third arg DELIMITED (prefix arg if interactive), if non-nil, means replace
only matches surrounded by word boundaries.  A negative prefix arg means
replace backward.

Operates on the region between START and END (if both are nil, from point
to the end of the buffer).  Interactively, if Transient Mark mode is
enabled and the mark is active, operates on the contents of the region;
otherwise from point to the end of the buffer's accessible portion.

Arguments BACKWARD and REGION-NONCONTIGUOUS-P are passed
to `perform-replace' (which see).

Use \\<minibuffer-local-map>\\[next-history-element] \
to pull the last incremental search string to the minibuffer
that reads FROM-STRING.

This function is usually the wrong thing to use in a Lisp program.
What you probably want is a loop like this:
  (while (search-forward FROM-STRING nil t)
    (replace-match TO-STRING nil t))
which will run faster and will not set the mark or print anything.
\(You may need a more complex loop if FROM-STRING can match the null string
and TO-STRING is also null.)"
  (declare (interactive-only
	    "use `search-forward' and `replace-match' instead.")
	   (interactive-args
	    (start (use-region-beginning))
	    (end (use-region-end))
	    (region-noncontiguous-p (use-region-noncontiguous-p))))
  (interactive
   (let ((common
	  (query-replace-read-args
	   (concat "Replace"
		   (if current-prefix-arg
		       (if (eq current-prefix-arg '-) " backward" " word")
		     "")
		   " string"
		   (if (use-region-p) " in region" ""))
	   nil)))
     (list (nth 0 common) (nth 1 common) (nth 2 common)
	   (use-region-beginning) (use-region-end)
	   (nth 3 common)
	   (use-region-noncontiguous-p))))
  (perform-replace from-string to-string nil nil delimited nil nil start end backward region-noncontiguous-p))

(defun replace-regexp (regexp to-string &optional delimited start end backward region-noncontiguous-p)
  "Replace things after point matching REGEXP with TO-STRING.
Preserve case in each match if `case-replace' and `case-fold-search'
are non-nil and REGEXP has no uppercase letters.

Ignore read-only matches if `query-replace-skip-read-only' is non-nil,
ignore hidden matches if `search-invisible' is nil, and ignore more
matches using `isearch-filter-predicate'.

If `replace-regexp-lax-whitespace' is non-nil, a space or spaces in the regexp
to be replaced will match a sequence of whitespace chars defined by the
regexp in `search-whitespace-regexp'.

This function is not affected by `replace-char-fold'

In Transient Mark mode, if the mark is active, operate on the contents
of the region.  Otherwise, operate from point to the end of the buffer's
accessible portion.

Third arg DELIMITED (prefix arg if interactive), if non-nil, means replace
only matches surrounded by word boundaries.  A negative prefix arg means
replace backward.

Fourth and fifth arg START and END specify the region to operate on.

Arguments BACKWARD and REGION-NONCONTIGUOUS-P are passed
to `perform-replace' (which see).

In TO-STRING, `\\&' stands for whatever matched the whole of REGEXP,
and `\\=\\N' (where N is a digit) stands for whatever matched
the Nth `\\(...\\)' (1-based) in REGEXP.
`\\?' lets you edit the replacement text in the minibuffer
at the given position for each replacement.

In interactive calls, the replacement text may contain `\\,'
followed by a Lisp expression used as part of the replacement
text.  Inside of that expression, `\\&' is a string denoting the
whole match, `\\N' a partial match, `\\#&' and `\\#N' the respective
numeric values from `string-to-number', and `\\#' itself for
`replace-count', the number of replacements occurred so far, starting
from zero.

If your Lisp expression is an identifier and the next letter in
the replacement string would be interpreted as part of it, you
can wrap it with an expression like `\\,(or \\#)'.  Incidentally,
for this particular case you may also enter `\\#' in the
replacement text directly.

When using those Lisp features interactively in the replacement
text, TO-STRING is actually made a list instead of a string.
Use \\[repeat-complex-command] after this command for details.

Use \\<minibuffer-local-map>\\[next-history-element] \
to pull the last incremental search regexp to the minibuffer
that reads REGEXP.

This function is usually the wrong thing to use in a Lisp program.
What you probably want is a loop like this:
  (while (re-search-forward REGEXP nil t)
    (replace-match TO-STRING nil nil))
which will run faster and will not set the mark or print anything."
  (declare (interactive-only
	    "use `re-search-forward' and `replace-match' instead.")
	   (interactive-args
	    (start (use-region-beginning))
	    (end (use-region-end))
	    (region-noncontiguous-p (use-region-noncontiguous-p))))
  (interactive
   (let ((common
	  (query-replace-read-args
	   (concat "Replace"
		   (if current-prefix-arg
		       (if (eq current-prefix-arg '-) " backward" " word")
		     "")
		   " regexp"
		   (if (use-region-p) " in region" ""))
	   t)))
     (list (nth 0 common) (nth 1 common) (nth 2 common)
	   (use-region-beginning) (use-region-end)
	   (nth 3 common)
	   (use-region-noncontiguous-p))))
  (perform-replace regexp to-string nil t delimited nil nil start end backward region-noncontiguous-p))


(defvar regexp-history nil
  "History list for some commands that read regular expressions.

Maximum length of the history list is determined by the value
of `history-length', which see.")

(defvar occur-highlight-overlays nil
  "Overlays used to temporarily highlight occur matches.")

(defvar occur-collect-regexp-history '("\\1")
  "History of regexp for occur's collect operation.")

(defcustom read-regexp-defaults-function nil
  "Function that provides default regexp(s) for `read-regexp'.
This function should take no arguments and return one of: nil, a
regexp, or a list of regexps.  Interactively, `read-regexp' uses
the return value of this function for its DEFAULT argument.

As an example, set this variable to `find-tag-default-as-regexp'
to default to the symbol at point.

To provide different default regexps for different commands,
the function that you set this to can check `this-command'."
  :type '(choice
	  (const :tag "No default regexp reading function" nil)
	  (const :tag "Latest regexp history" regexp-history-last)
	  (function-item :tag "Tag at point"
			 find-tag-default)
	  (function-item :tag "Tag at point as regexp"
			 find-tag-default-as-regexp)
	  (function-item :tag "Tag at point as symbol regexp"
			 find-tag-default-as-symbol-regexp)
	  (function :tag "Your choice of function"))
  :group 'matching
  :version "24.4")

(defun read-regexp-suggestions ()
  "Return a list of standard suggestions for `read-regexp'.
By default, the list includes the active region, the identifier
(a.k.a. \"tag\") at point (see Info node `(emacs) Identifier Search'),
the last isearch regexp, the last isearch string, and the last
replacement regexp.  `read-regexp' appends the list returned
by this function to the end of values available via
\\<minibuffer-local-map>\\[next-history-element]."
  (list
   (when (use-region-p)
     (buffer-substring-no-properties
      (region-beginning) (region-end)))
   (find-tag-default-as-regexp)
   (find-tag-default-as-symbol-regexp)
   (car regexp-search-ring)
   (regexp-quote (or (car search-ring) ""))
   (car (symbol-value query-replace-from-history-variable))))

(defvar-keymap read-regexp-map
  :parent minibuffer-local-map
  "M-s c" #'read-regexp-toggle-case-fold)

(defvar read-regexp--case-fold nil)

(defun read-regexp-toggle-case-fold ()
  (interactive)
  (setq read-regexp--case-fold
        (if (or (eq read-regexp--case-fold 'fold)
                (and read-regexp--case-fold
                     (not (eq read-regexp--case-fold 'inhibit-fold))))
            'inhibit-fold
          'fold))
  (minibuffer-message "Case folding is now %s"
                      (if (eq read-regexp--case-fold 'fold) "on" "off")))

(defun read-regexp (prompt &optional defaults history)
  "Read and return a regular expression as a string.
Prompt with the string PROMPT.  If PROMPT ends in \":\" (followed by
optional whitespace), use it as-is.  Otherwise, add \": \" to the end,
possibly preceded by the default result (see below).

The optional argument DEFAULTS is used to construct the default
return value in case of empty input.  DEFAULTS can be nil, a string,
a list of strings, or a symbol.

If DEFAULTS is a string, the function uses it as-is.

If DEFAULTS is a list of strings, the first element is the
default return value, but all the elements are accessible
using the history command \\<minibuffer-local-map>\\[next-history-element].

If DEFAULTS is the symbol `regexp-history-last', the default return
value will be the first element of HISTORY.  If HISTORY is omitted or
nil, `regexp-history' is used instead.
If DEFAULTS is a symbol with a function definition, it is called with
no arguments and should return either nil, a string, or a list of
strings, which will be used as above.
Other symbol values for DEFAULTS are ignored.

If `read-regexp-defaults-function' is non-nil, its value is used
instead of DEFAULTS in the two cases described in the last paragraph.

Before using whatever value DEFAULTS yields, the function appends the
standard values from `read-regexp-suggestions' to that value.

If the first element of DEFAULTS is non-nil (and if PROMPT does not end
in \":\", followed by optional whitespace), DEFAULT is added to the prompt.

The optional argument HISTORY is a symbol to use for the history list.
If nil, use `regexp-history'.

If the user has used the \\<read-regexp-map>\\[read-regexp-toggle-case-fold] command to specify case
sensitivity, the returned string will have a text property named
`case-fold' that has a value of either `fold' or
`inhibit-fold'.  (It's up to the caller of `read-regexp' to
respect this or not; see `read-regexp-case-fold-search'.)

This command uses the `read-regexp-map' keymap while reading the
regexp from the user."
  (let* ((defaults
	   (if (and defaults (symbolp defaults))
	       (cond
		((eq (or read-regexp-defaults-function defaults)
		     'regexp-history-last)
		 (car (symbol-value (or history 'regexp-history))))
		((functionp (or read-regexp-defaults-function defaults))
		 (funcall (or read-regexp-defaults-function defaults))))
	     defaults))
	 (default     (if (consp defaults) (car defaults) defaults))
	 (suggestions (if (listp defaults) defaults (list defaults)))
	 (suggestions (append suggestions (read-regexp-suggestions)))
	 (suggestions (delete-dups (delq nil (delete "" suggestions))))
	 ;; Do not automatically add default to the history for empty input.
	 (history-add-new-input nil)
         ;; `read-regexp--case-fold' dynamically bound and may be
         ;; altered by `M-c'.
         (read-regexp--case-fold case-fold-search)
	 (input (read-from-minibuffer
                 (if (string-match-p ":[ \t]*\\'" prompt)
                     prompt
                   (format-prompt prompt (and (length> default 0)
                                              (query-replace-descr default))))
		 nil read-regexp-map
                 nil (or history 'regexp-history) suggestions t))
         (result (if (equal input "")
	             ;; Return the default value when the user enters
	             ;; empty input.
                     default
                   input)))
    (when result
      (add-to-history (or history 'regexp-history) result))
    (if (and result
             (or (eq read-regexp--case-fold 'fold)
                 (eq read-regexp--case-fold 'inhibit-fold)))
        (propertize result 'case-fold read-regexp--case-fold)
      (or result input))))

(defun read-regexp-case-fold-search (regexp)
  "Return the value for `case-fold-search' based on REGEXP and current settings.
REGEXP is a string as returned by `read-regexp'."
  (let ((fold (get-text-property 0 'case-fold regexp)))
    (cond
     ((eq fold 'fold) t)
     ((eq fold 'inhibit-fold) nil)
     (t case-fold-search))))

(defalias 'delete-non-matching-lines 'keep-lines)
(defalias 'delete-matching-lines 'flush-lines)
(defalias 'count-matches 'how-many)

(defun keep-lines-read-args (prompt)
  "Read arguments for `keep-lines' and friends.
Prompt for a regexp with PROMPT.
Value is a list, (REGEXP)."
  (list (read-regexp prompt) nil nil t))

(defun keep-lines (regexp &optional rstart rend interactive)
  "Delete all lines except those containing matches for REGEXP.
A match split across lines preserves all the lines it lies in.
When called from Lisp (and usually interactively as well, see below)
applies to all lines starting after point.

If REGEXP contains upper case characters (excluding those preceded by `\\')
and `search-upper-case' is non-nil, the matching is case-sensitive.

Second and third arg RSTART and REND specify the region to operate on.
This command operates on (the accessible part of) all lines whose
accessible part is entirely contained in the region determined by RSTART
and REND.  (A newline ending a line counts as part of that line.)  If RSTART
is non-nil, REND also has to be given.

Interactively, in Transient Mark mode when the mark is active, operate
on all lines whose accessible part is entirely contained in the region.
Otherwise, the command applies to all lines starting after point.
When calling this function from Lisp, you can pretend that it was
called interactively by passing a non-nil INTERACTIVE argument.

This function starts looking for the next match from the end of
the previous match.  Hence, it ignores matches that overlap
a previously found match."
  (interactive
   (progn
     (barf-if-buffer-read-only)
     (keep-lines-read-args "Keep lines containing match for regexp")))
  (if rstart
      (progn
	(goto-char (min rstart rend))
	(setq rend
	      (progn
		(save-excursion
		  (goto-char (max rstart rend))
		  (unless (or (bolp) (eobp))
		    (forward-line 0))
		  (point-marker)))))
    (if (and interactive (use-region-p))
	(setq rstart (region-beginning)
	      rend (progn
		     (goto-char (region-end))
		     (unless (or (bolp) (eobp))
		       (forward-line 0))
		     (point-marker)))
      (setq rstart (point)
	    rend (point-max-marker)))
    (goto-char rstart))
  (save-excursion
    (or (bolp) (forward-line 1))
    (let ((start (point))
	  (case-fold-search
	   (if (and case-fold-search search-upper-case)
	       (isearch-no-upper-case-p regexp t)
	     case-fold-search)))
      (while (< (point) rend)
	;; Start is first char not preserved by previous match.
	(if (not (re-search-forward regexp rend 'move))
	    (delete-region start rend)
	  (let ((end (save-excursion (goto-char (match-beginning 0))
				     (forward-line 0)
				     (point))))
	    ;; Now end is first char preserved by the new match.
	    (if (< start end)
		(delete-region start end))))

	(setq start (save-excursion (forward-line 1) (point)))
	;; If the match was empty, avoid matching again at same place.
	(and (< (point) rend)
	     (= (match-beginning 0) (match-end 0))
	     (forward-char 1)))))
  (set-marker rend nil)
  nil)

(defun flush-lines (regexp &optional rstart rend interactive)
  "Delete lines containing matches for REGEXP.
When called from Lisp (and usually when called interactively as
well, see below), applies to the part of the buffer after point.
The line point is in is deleted if and only if it contains a
match for regexp starting after point.

If REGEXP contains upper case characters (excluding those preceded by `\\')
and `search-upper-case' is non-nil, the matching is case-sensitive.

Second and third arg RSTART and REND specify the region to operate on.
Lines partially contained in this region are deleted if and only if
they contain a match entirely contained in it.

Interactively, in Transient Mark mode when the mark is active, operate
on the contents of the region.  Otherwise, operate from point to the
end of (the accessible portion of) the buffer.  When calling this function
from Lisp, you can pretend that it was called interactively by passing
a non-nil INTERACTIVE argument.

If a match is split across lines, all the lines it lies in are deleted.
They are deleted _before_ looking for the next match.  Hence, a match
starting on the same line at which another match ended is ignored.

Return the number of deleted matching lines.  When called interactively,
also print the number.

If you want to not just delete the lines, but also add them to
the kill ring, use the \\[kill-matching-lines] command instead."
  (interactive
   (progn
     (barf-if-buffer-read-only)
     (keep-lines-read-args "Flush lines containing match for regexp")))
  (if rstart
      (progn
	(goto-char (min rstart rend))
	(setq rend (copy-marker (max rstart rend))))
    (if (and interactive (use-region-p))
	(setq rstart (region-beginning)
	      rend (copy-marker (region-end)))
      (setq rstart (point)
	    rend (point-max-marker)))
    (goto-char rstart))
  (let ((count 0)
        (case-fold-search
	 (if (and case-fold-search search-upper-case)
	     (isearch-no-upper-case-p regexp t)
	   case-fold-search)))
    (save-excursion
      (while (and (< (point) rend)
		  (re-search-forward regexp rend t))
	(delete-region (save-excursion (goto-char (match-beginning 0))
				       (forward-line 0)
				       (point))
		       (progn (forward-line 1) (point)))
        (setq count (1+ count))))
    (set-marker rend nil)
    (when interactive (message (ngettext "Deleted %d matching line"
					 "Deleted %d matching lines"
					 count)
			       count))
    count))

(defun kill-matching-lines (regexp &optional rstart rend interactive)
  "Kill lines containing matches for REGEXP.

When called from Lisp (and usually when called interactively as
well, see below), applies to the part of the buffer after point.
The line point is in is killed if and only if it contains a match
for REGEXP starting after point.

If REGEXP contains upper case characters (excluding those
preceded by `\\') and `search-upper-case' is non-nil, the
matching is case-sensitive.

Second and third args RSTART and REND specify the region to
operate on.  Lines partially contained in this region are killed
if and only if they contain a match entirely contained in the
region.

Interactively, in Transient Mark mode when the mark is active,
operate on the contents of the region.  Otherwise, operate from
point to the end of (the accessible portion of) the buffer.

If a match is split across lines, all the lines it lies in are
killed.  They are killed _before_ looking for the next match.
Hence, a match starting on the same line at which another match
ended is ignored.

Return the number of killed matching lines.  When called
interactively, also print the number.

If you merely want to delete the lines, without adding them to
the kill ring, the \\[delete-matching-lines] command is faster."
  (interactive
   (progn
     (barf-if-buffer-read-only)
     (keep-lines-read-args "Kill lines containing match for regexp")))
  (if rstart
      (progn
	(goto-char (min rstart rend))
	(setq rend (copy-marker (max rstart rend))))
    (if (and interactive (use-region-p))
	(setq rstart (region-beginning)
	      rend (copy-marker (region-end)))
      (setq rstart (point)
	    rend (point-max-marker)))
    (goto-char rstart))
  (let ((count 0)
        (case-fold-search
	 (if (and case-fold-search search-upper-case)
	     (isearch-no-upper-case-p regexp t)
	   case-fold-search)))
    (save-excursion
      (while (and (< (point) rend)
		  (re-search-forward regexp rend t))
        (unless (zerop count)
          (setq last-command 'kill-region))
	(kill-region (save-excursion (goto-char (match-beginning 0))
                                     (forward-line 0)
                                     (point))
                     (progn (forward-line 1) (point)))
        (setq count (1+ count))))
    (set-marker rend nil)
    (when interactive (message (ngettext "Killed %d matching line"
					 "Killed %d matching lines"
					 count)
			       count))
    count))

(defun copy-matching-lines (regexp &optional rstart rend interactive)
  "Copy lines containing matches for REGEXP to the kill ring.

When called from Lisp (and usually when called interactively as
well, see below), applies to the part of the buffer after point.
The line point is in is copied if and only if it contains a match
for REGEXP starting after point.

If REGEXP contains upper case characters (excluding those
preceded by `\\') and `search-upper-case' is non-nil, the
matching is case-sensitive.

Second and third args RSTART and REND specify the region to
operate on.  Lines partially contained in this region are copied
if and only if they contain a match entirely contained in the
region.

Interactively, in Transient Mark mode when the mark is active,
operate on the contents of the region.  Otherwise, operate from
point to the end of (the accessible portion of) the buffer.

If a match is split across lines, all the lines it lies in are
copied.

Return the number of copied matching lines.  When called
interactively, also print the number."
  (interactive
   (keep-lines-read-args "Copy lines containing match for regexp"))
  (if rstart
      (progn
	(goto-char (min rstart rend))
	(setq rend (copy-marker (max rstart rend))))
    (if (and interactive (use-region-p))
	(setq rstart (region-beginning)
	      rend (copy-marker (region-end)))
      (setq rstart (point)
	    rend (point-max-marker)))
    (goto-char rstart))
  (let ((count 0)
        (case-fold-search
	 (if (and case-fold-search search-upper-case)
	     (isearch-no-upper-case-p regexp t)
	   case-fold-search)))
    (save-excursion
      (while (and (< (point) rend)
		  (re-search-forward regexp rend t))
	(unless (zerop count)
          (setq last-command 'kill-region))
	(copy-region-as-kill (save-excursion (goto-char (match-beginning 0))
                                             (forward-line 0)
                                             (point))
                             (progn (forward-line 1) (point)))
        (setq count (1+ count))))
    (set-marker rend nil)
    (when interactive (message (ngettext "Copied %d matching line"
					 "Copied %d matching lines"
					 count)
			       count))
    count))

(defun how-many (regexp &optional rstart rend interactive)
  "Print and return number of matches for REGEXP following point.
When called from Lisp and INTERACTIVE is omitted or nil, just return
the number, do not print it; if INTERACTIVE is t, the function behaves
in all respects as if it had been called interactively.

If REGEXP contains upper case characters (excluding those preceded by `\\')
and `search-upper-case' is non-nil, the matching is case-sensitive.

Second and third arg RSTART and REND specify the region to operate on.

Interactively, in Transient Mark mode when the mark is active, operate
on the contents of the region.  Otherwise, operate from point to the
end of (the accessible portion of) the buffer.

This function starts looking for the next match from the end of
the previous match.  Hence, it ignores matches that overlap
a previously found match."
  (interactive
   (keep-lines-read-args "How many matches for regexp"))
  (save-excursion
    (if rstart
        (if rend
            (progn
              (goto-char (min rstart rend))
              (setq rend (max rstart rend)))
          (goto-char rstart)
          (setq rend (point-max)))
      (if (and interactive (use-region-p))
	  (setq rstart (region-beginning)
		rend (region-end))
	(setq rstart (point)
	      rend (point-max)))
      (goto-char rstart))
    (let ((count 0)
	  (case-fold-search
	   (if (and case-fold-search search-upper-case)
	       (isearch-no-upper-case-p regexp t)
	     case-fold-search)))
      (while (and (< (point) rend)
		  (re-search-forward regexp rend t))
        ;; Ensure forward progress on zero-length matches like "^$".
        (when (and (= (match-beginning 0) (match-end 0))
                   (not (eobp)))
          (forward-char 1))
	(setq count (1+ count)))
      (when interactive (message (ngettext "%d occurrence"
					   "%d occurrences"
					   count)
				 count))
      count)))


(easy-menu-define occur-menu-map nil
  "Menu for `occur-mode'."
  '("Occur"
    ["Move to Previous Match" occur-prev
     :help "Move to the Nth (default 1) previous match in an Occur mode buffer"]
    ["Move to Next Match" occur-next
     :help "Move to the Nth (default 1) next match in an Occur mode buffer"]
    ["Display Occurrence" occur-mode-display-occurrence
     :help "Display in another window the occurrence the current line describes"]
    ["Go To Occurrence" occur-mode-goto-occurrence
     :help "Go to the occurrence the current line describes"]
    ["Go To Occurrence Other Window" occur-mode-goto-occurrence-other-window
     :help "Go to the occurrence the current line describes, in another window"]
    "---"
    ["Edit Occur Buffer" occur-edit-mode
     :help "Edit the *Occur* buffer and apply changes to the original buffers."]
    ["Rename Occur Buffer" occur-rename-buffer
     :help "Rename the current *Occur* buffer to *Occur: original-buffer-name*."]
    ["Clone Occur Buffer" clone-buffer
     :help "Create and return a twin copy of the current *Occur* buffer"]
    ["Revert Occur Buffer" revert-buffer
     :help "Replace the text in the *Occur* buffer with the results of rerunning occur"]
    ["Quit Occur Window" quit-window
     :help "Quit the current *Occur* buffer.  Bury it, and maybe delete the selected frame"]
    ["Kill Occur Buffer" kill-this-buffer
     :help "Kill the current *Occur* buffer"]
    "---"
    ["Auto Occurrence Display"
     next-error-follow-minor-mode
     :help "Display another occurrence when moving the cursor"
     :style toggle
     :selected (and (boundp 'next-error-follow-minor-mode)
                    next-error-follow-minor-mode)]))

(defvar occur-mode-map
  (let ((map (make-sparse-keymap)))
    ;; We use this alternative name, so we can use \\[occur-mode-mouse-goto].
    (define-key map [mouse-2] 'occur-mode-mouse-goto)
    (define-key map "\C-c\C-c" 'occur-mode-goto-occurrence)
    (define-key map "e" 'occur-edit-mode)
    (define-key map "\C-m" 'occur-mode-goto-occurrence)
    (define-key map "o" 'occur-mode-goto-occurrence-other-window)
    (define-key map "\C-o" 'occur-mode-display-occurrence)
    (define-key map "n" 'next-error-no-select)
    (define-key map "p" 'previous-error-no-select)
    (define-key map "l" 'recenter-current-error)
    (define-key map "\M-n" 'occur-next)
    (define-key map "\M-p" 'occur-prev)
    (define-key map "r" 'occur-rename-buffer)
    (define-key map "c" 'clone-buffer)
    (define-key map "\C-c\C-f" 'next-error-follow-minor-mode)
    (define-key map [menu-bar occur] (cons "Occur" occur-menu-map))
    map)
  "Keymap for `occur-mode'.")

(defvar-local occur-revert-arguments nil
  "Arguments to pass to `occur-1' to revert an Occur mode buffer.
See `occur-revert-function'.")
(put 'occur-revert-arguments 'permanent-local t)

(defcustom occur-mode-hook '(turn-on-font-lock)
  "Hook run when entering Occur mode."
  :type 'hook
  :group 'matching)

(defcustom occur-hook nil
  "Hook run by Occur when there are any matches."
  :type 'hook
  :group 'matching)

(defcustom occur-mode-find-occurrence-hook nil
  "Hook run by Occur after locating an occurrence.
This will be called with the cursor position at the occurrence.  An application
for this is to reveal context in an outline mode when the occurrence is hidden."
  :type 'hook
  :group 'matching)

(defun occur--garbage-collect-revert-args ()
  (dolist (boo (nth 2 occur-revert-arguments))
    (when (overlayp boo) (delete-overlay boo)))
  (kill-local-variable 'occur-revert-arguments))

(put 'occur-mode 'mode-class 'special)
(define-derived-mode occur-mode special-mode "Occur"
  "Major mode for output from \\[occur].
\\<occur-mode-map>Move point to one of the items in this buffer, then use
\\[occur-mode-goto-occurrence] to go to the occurrence that the item refers to.
Alternatively, click \\[occur-mode-mouse-goto] on an item to go to it.

\\{occur-mode-map}"
  (setq-local revert-buffer-function #'occur-revert-function)
  (add-hook 'kill-buffer-hook #'occur--garbage-collect-revert-args nil t)
  (setq next-error-function #'occur-next-error))


;;; Occur Edit mode

(defvar occur-edit-mode-map
  (let ((map (make-sparse-keymap)))
    (set-keymap-parent map text-mode-map)
    (define-key map [mouse-2] 'occur-mode-mouse-goto)
    (define-key map "\C-c\C-c" 'occur-cease-edit)
    (define-key map "\C-o" 'occur-mode-display-occurrence)
    (define-key map "\C-c\C-f" 'next-error-follow-minor-mode)
    (define-key map [menu-bar occur] (cons "Occur" occur-menu-map))
    map)
  "Keymap for `occur-edit-mode'.")

(define-derived-mode occur-edit-mode occur-mode "Occur-Edit"
  "Major mode for editing *Occur* buffers.
In this mode, changes to the *Occur* buffer are also applied to
the originating buffer.

To return to ordinary Occur mode, use \\[occur-cease-edit]."
  (setq buffer-read-only nil)
  (add-hook 'after-change-functions #'occur-after-change-function nil t)
  (message (substitute-command-keys
	    "Editing: Type \\[occur-cease-edit] to return to Occur mode.")))

(defun occur-cease-edit ()
  "Switch from Occur Edit mode to Occur mode."
  (interactive)
  (when (derived-mode-p 'occur-edit-mode)
    (occur-mode)
    (message "Switching to Occur mode.")))

(defun occur--targets-start (targets)
  "First marker of the `occur-target' property value TARGETS."
  (if (consp targets)
      (caar targets)
    ;; Tolerate an `occur-target' value that is a single marker for
    ;; compatibility.
    targets))

(defun occur-after-change-function (beg end length)
  (save-excursion
    (goto-char beg)
    (let* ((line-beg (line-beginning-position))
	   (targets (get-text-property line-beg 'occur-target))
           (m (occur--targets-start targets))
	   (buf (marker-buffer m))
	   col)
      (when (and (get-text-property line-beg 'occur-prefix)
		 (not (get-text-property end 'occur-prefix)))
	(when (= length 0)
	  ;; Apply occur-target property to inserted (e.g. yanked) text.
	  (put-text-property beg end 'occur-target targets)
	  ;; Did we insert a newline?  Occur Edit mode can't create new
	  ;; Occur entries; just discard everything after the newline.
	  (save-excursion
	    (and (search-forward "\n" end t)
		 (delete-region (1- (point)) end))))
	(let* ((line (- (line-number-at-pos)
			(line-number-at-pos (window-start))))
	       (readonly (with-current-buffer buf buffer-read-only))
	       (win (or (get-buffer-window buf)
			(display-buffer buf
					'(nil (inhibit-same-window . t)
					      (inhibit-switch-frame . t)))))
	       (line-end (line-end-position))
	       (text (save-excursion
		       (goto-char (next-single-property-change
				   line-beg 'occur-prefix nil
				   line-end))
		       (setq col (- (point) line-beg))
		       (buffer-substring-no-properties (point) line-end))))
	  (with-selected-window win
	    (goto-char m)
	    (recenter line)
	    (if readonly
		(message "Buffer `%s' is read only." buf)
              ;; Replace the line, but make the change as small as
              ;; possible by shrink-wrapping.  That way, we avoid
              ;; disturbing markers unnecessarily.
              (let* ((beg-pos (line-beginning-position))
                     (end-pos (line-end-position))
                     (buf-str (buffer-substring-no-properties beg-pos end-pos))
                     (common-prefix
                      (lambda (s1 s2)
                        (let ((c (compare-strings s1 nil nil s2 nil nil)))
                          (if (numberp c)
                              (1- (abs c))
                            (length s1)))))
                     (prefix-len (funcall common-prefix buf-str text))
                     (suffix-len (funcall common-prefix
                                          (reverse (substring
                                                    buf-str prefix-len))
                                          (reverse (substring
                                                    text prefix-len)))))
                (setq beg-pos (+ beg-pos prefix-len))
                (setq end-pos (- end-pos suffix-len))
                (setq text (substring text prefix-len
                                      (and (not (zerop suffix-len))
                                           (- suffix-len))))
                (delete-region beg-pos end-pos)
                (goto-char beg-pos)
                (insert text)))
	    (move-to-column col)))))))


(defun occur-revert-function (_ignore1 _ignore2)
  "Handle `revert-buffer' for Occur mode buffers."
  (apply #'occur-1 (append occur-revert-arguments (list (buffer-name)))))

;; Retained for compatibility.
(defun occur-mode-find-occurrence ()
  "Return a marker to the first match of the line at point."
  (occur--targets-start (occur-mode--find-occurrences)))

(defun occur-mode--find-occurrences ()
  ;; The `occur-target' property value is a list of (BEG . END) for each
  ;; match on the line, or (for compatibility) a single marker to the start
  ;; of the first match.
  (let* ((targets (get-text-property (point) 'occur-target))
         (start (occur--targets-start targets)))
    (unless targets
      (error "No occurrence on this line"))
    (unless (buffer-live-p (marker-buffer start))
      (error "Buffer for this occurrence was killed"))
    targets))

(defun occur--set-arrow ()
  "Set the overlay arrow at the first line of the occur match at point."
  (save-excursion
    (let ((target (get-text-property (point) 'occur-target))
          ;; Find the start of the occur match, in case it's multi-line.
          (prev (previous-single-property-change (point) 'occur-target)))
      (when (and prev (eq (get-text-property prev 'occur-target) target))
        (goto-char prev))
      (setq overlay-arrow-position
            (set-marker (or overlay-arrow-position (make-marker))
                        (line-beginning-position))))))

(defalias 'occur-mode-mouse-goto 'occur-mode-goto-occurrence)
(defun occur-mode-goto-occurrence (&optional event)
  "Go to the occurrence specified by EVENT, a mouse click.
If not invoked by a mouse click, go to occurrence on the current line."
  (interactive (list last-nonmenu-event))
  (let* ((buffer (when event (current-buffer)))
         (targets
          (if (null event)
              ;; Actually `event-end' works correctly with a nil argument as
              ;; well, so we could dispense with this test, but let's not
              ;; rely on this undocumented behavior.
              (occur-mode--find-occurrences)
            (with-current-buffer (window-buffer (posn-window (event-end event)))
              (save-excursion
                (goto-char (posn-point (event-end event)))
                (occur-mode--find-occurrences)))))
         (pos (occur--targets-start targets)))
    (occur--set-arrow)
    (pop-to-buffer (marker-buffer pos))
    (goto-char pos)
    (occur--highlight-occurrences targets)
    (when buffer (next-error-found buffer (current-buffer)))
    (run-hooks 'occur-mode-find-occurrence-hook)))

(defun occur-mode-goto-occurrence-other-window ()
  "Go to the occurrence the current line describes, in another window."
  (interactive)
  (let ((buffer (current-buffer))
        (pos (occur--targets-start (occur-mode--find-occurrences))))
    (occur--set-arrow)
    (switch-to-buffer-other-window (marker-buffer pos))
    (goto-char pos)
    (next-error-found buffer (current-buffer))
    (run-hooks 'occur-mode-find-occurrence-hook)))

(defun occur-goto-locus-delete-o ()
  (mapc #'delete-overlay occur-highlight-overlays)
  (setq occur-highlight-overlays nil)
  ;; Get rid of timer and hook that would try to do this again.
  (if (timerp next-error-highlight-timer)
      (cancel-timer next-error-highlight-timer))
  (remove-hook 'pre-command-hook
               #'occur-goto-locus-delete-o))

;; Highlight the current visited occurrence.
(defun occur--highlight-occurrences (targets)
  (let ((start-marker (occur--targets-start targets)))
    (occur-goto-locus-delete-o)
    (with-current-buffer (marker-buffer start-marker)
      (when (or (eq next-error-highlight t)
	        (numberp next-error-highlight))
        (setq occur-highlight-overlays
              (mapcar (lambda (target)
                        (let ((o (make-overlay (car target) (cdr target))))
                          (overlay-put o 'face 'next-error)
                          o))
                      (if (listp targets)
                          targets
                        ;; `occur-target' compatibility: when we only
                        ;; have a single starting point, highlight the
                        ;; rest of the line.
                        (let ((end-pos (save-excursion
                                         (goto-char start-marker)
                                         (line-end-position))))
                          (list (cons start-marker end-pos))))))
        (add-hook 'pre-command-hook #'occur-goto-locus-delete-o)
        (when (numberp next-error-highlight)
          ;; We want highlighting for a limited time:
          ;; set up a timer to delete it.
	  (setq next-error-highlight-timer
	        (run-at-time next-error-highlight nil
			     'occur-goto-locus-delete-o))))

      (when (eq next-error-highlight 'fringe-arrow)
        ;; We want a fringe arrow (instead of highlighting).
        (setq next-error-overlay-arrow-position
	      (copy-marker (line-beginning-position)))))))

(defun occur-mode-display-occurrence ()
  "Display in another window the occurrence the current line describes."
  (interactive)
  (let* ((buffer (current-buffer))
         (targets (occur-mode--find-occurrences))
         (pos (occur--targets-start targets))
         (next-error-highlight next-error-highlight-no-select)
         (display-buffer-overriding-action
          '(nil (inhibit-same-window . t)))
	 window)
    (setq window (display-buffer (marker-buffer pos) t))
    (occur--set-arrow)
    ;; This is the way to set point in the proper window.
    (save-selected-window
      (select-window window)
      (goto-char pos)
      (occur--highlight-occurrences targets)
      (next-error-found buffer (current-buffer))
      (run-hooks 'occur-mode-find-occurrence-hook))))

(defun occur-find-match (n search message)
  (if (not n) (setq n 1))
  (let ((r))
    (while (> n 0)
      (setq r (funcall search (point) 'occur-match))
      (and r
           (get-text-property r 'occur-match)
           (setq r (funcall search r 'occur-match)))
      (if r
          (goto-char r)
        (user-error message))
      (setq n (1- n)))))

(defun occur-next (&optional n)
  "Move to the Nth (default 1) next match in an Occur mode buffer."
  (interactive "p")
  (occur-find-match n #'next-single-property-change "No more matches"))

(defun occur-prev (&optional n)
  "Move to the Nth (default 1) previous match in an Occur mode buffer."
  (interactive "p")
  (occur-find-match n #'previous-single-property-change "No earlier matches"))

(defun occur-next-error (&optional argp reset)
  "Move to the ARGPth (default 1) next match in an Occur mode buffer.
RESET non-nil means rewind to the first match.
This is a compatibility function for \\[next-error] invocations."
  (interactive "p")
  (goto-char (cond (reset (point-min))
		   ((< argp 0) (line-beginning-position))
		   ((> argp 0) (line-end-position))
		   ((point))))
  (occur-find-match
   (abs argp)
   (if (> 0 argp)
       #'previous-single-property-change
     #'next-single-property-change)
   "No more matches")
  ;; In case the *Occur* buffer is visible in a nonselected window.
  (let ((win (get-buffer-window (current-buffer) t)))
    (if win (set-window-point win (point))))
  (occur-mode-goto-occurrence))

(defface match
  '((((class color) (min-colors 88) (background light))
     :background "khaki1")
    (((class color) (min-colors 88) (background dark))
     :background "RoyalBlue3")
    (((class color) (min-colors 8) (background light))
     :background "yellow" :foreground "black")
    (((class color) (min-colors 8) (background dark))
     :background "blue" :foreground "white")
    (((type tty) (class mono))
     :inverse-video t)
    (t :background "gray"))
  "Face used to highlight matches permanently."
  :group 'matching
  :group 'basic-faces
  :version "22.1")

(defcustom list-matching-lines-default-context-lines 0
  "Default number of context lines included around `list-matching-lines' matches.
A negative number means to include that many lines before the match.
A positive number means to include that many lines both before and after."
  :type 'integer
  :group 'matching)

(defalias 'list-matching-lines 'occur)

(defcustom list-matching-lines-face 'match
  "Face used by \\[list-matching-lines] to show the text that matches.
If the value is nil, don't highlight the matching portions specially."
  :type '(choice (const :tag "Don't highlight matching portions" nil)
                 face)
  :group 'matching)

(defcustom list-matching-lines-buffer-name-face 'underline
  "Face used by \\[list-matching-lines] to show the names of buffers.
If the value is nil, don't highlight the buffer names specially."
  :type '(choice (const :tag "Don't highlight buffer names" nil)
                 face)
  :group 'matching)

(defcustom list-matching-lines-current-line-face 'lazy-highlight
  "Face used by \\[list-matching-lines] to highlight the current line."
  :type 'face
  :group 'matching
  :version "26.1")

(defcustom list-matching-lines-jump-to-current-line nil
  "If non-nil, \\[list-matching-lines] shows the current line highlighted.
The current line for this purpose is the line of the original buffer
which was current when \\[list-matching-lines] was invoked.
Point in the `*Occur*' buffer will be set right after such line when
there are matches after it."
:type 'boolean
:group 'matching
:version "26.1")

(defcustom list-matching-lines-prefix-face 'shadow
  "Face used by \\[list-matching-lines] to show the prefix column.
The prefix column is the part of display that precedes the actual
contents of the line; it normally shows the line number.  \(For
multiline matches, the prefix column shows the line number for the
first line and whitespace for the rest of the lines.\)
If this face will display the same as the default face, the prefix
column will not be highlighted specially."
  :type 'face
  :group 'matching
  :version "24.4")

(defcustom occur-excluded-properties
  '(read-only invisible intangible field mouse-face help-echo local-map keymap
    yank-handler follow-link)
  "Text properties to discard when copying lines to the *Occur* buffer.
The value should be a list of text properties to discard or t,
which means to discard all text properties."
  :type '(choice (const :tag "All" t) (repeat symbol))
  :group 'matching
  :version "22.1")

(defun occur-read-primary-args ()
  (let* ((perform-collect (consp current-prefix-arg))
         (regexp (read-regexp (if perform-collect
                                  "Collect strings matching regexp"
                                "List lines matching regexp")
                              'regexp-history-last)))
    (list regexp
	  (if perform-collect
	      ;; Perform collect operation
	      (if (zerop (regexp-opt-depth regexp))
		  ;; No subexpression so collect the entire match.
		  "\\&"
		;; Get the regexp for collection pattern.
		(let ((default (car occur-collect-regexp-history)))
		  (read-regexp
		   (format-prompt "Regexp to collect" default)
		   default 'occur-collect-regexp-history)))
	    ;; Otherwise normal occur takes numerical prefix argument.
	    (when current-prefix-arg
	      (prefix-numeric-value current-prefix-arg))))))

(defun occur-rename-buffer (&optional unique-p interactive-p)
  "Rename the current *Occur* buffer to *Occur: original-buffer-name*.
Here `original-buffer-name' is the buffer name where Occur was originally run.
If UNIQUE-P is non-nil (interactively, the prefix argument), or called
non-interactively with INTERACTIVE-P nil, the renaming will not clobber
the existing buffer(s) of that name, but will use `generate-new-buffer-name'
instead.
You can add this to `occur-hook' if you always want a separate
*Occur* buffer for each buffer where you invoke `occur'."
  (interactive "P\np")
  (with-current-buffer
      (if (eq major-mode 'occur-mode) (current-buffer) (get-buffer "*Occur*"))
    (rename-buffer (concat "*Occur: "
                           (mapconcat (lambda (boo)
                                        (buffer-name (if (overlayp boo)
                                                         (overlay-buffer boo)
                                                       boo)))
                                      (car (cddr occur-revert-arguments)) "/")
                           "*")
                   (or unique-p (not interactive-p)))))

;; Region limits when `occur' applies on a region.
(defvar occur--final-pos nil)

(defun occur (regexp &optional nlines region)
  "Show all lines in the current buffer containing a match for REGEXP.
If a match spreads across multiple lines, all those lines are shown.

Each match is extended to include complete lines.  Only non-overlapping
matches are considered.  (Note that extending matches to complete
lines could cause some of the matches to overlap; if so, they will not
be shown as separate matches.)

Each line is displayed with NLINES lines before and after, or -NLINES
before if NLINES is negative.
NLINES defaults to `list-matching-lines-default-context-lines'.
Interactively it is the prefix arg.

Optional arg REGION, if non-nil, mean restrict search to the
specified region.  Otherwise search the entire buffer.
REGION must be a list of (START . END) positions as returned by
`region-bounds'.

The lines are shown in a buffer named `*Occur*'.
That buffer can serve as a menu for finding any of the matches for REGEXP
in the current buffer.
\\<occur-mode-map>\\[describe-mode] in that buffer will explain how.

Matches for REGEXP are shown in the face determined by the
variable `list-matching-lines-face'.
Names of buffers with matched lines are shown in the face determined
by the variable `list-matching-lines-buffer-name-face'.
The line numbers of the matching lines are shown in the face
determined by the variable `list-matching-lines-prefix-face'.

If `list-matching-lines-jump-to-current-line' is non-nil, then the
line in the current buffer which was current when the command was
invoked will be shown in the `*Occur*' buffer highlighted with
the `list-matching-lines-current-line-face', with point at the end
of that line.  (If the current line doesn't match REGEXP, it will
nonetheless be inserted into the `*Occur*' buffer between the 2
closest lines that do match REGEXP.)

If REGEXP contains upper case characters (excluding those preceded by `\\')
and `search-upper-case' is non-nil, the matching is case-sensitive.

When NLINES is a string or when the function is called
interactively with prefix argument without a number (\\[universal-argument] alone
as prefix) the matching strings are collected into the `*Occur*'
buffer by using NLINES as a replacement regexp.  NLINES may
contain \\& and \\N which convention follows `replace-match'.
For example, providing \"defun\\s +\\(\\S +\\)\" for REGEXP and
\"\\1\" for NLINES collects all the function names in a lisp
program.  When there is no parenthesized subexpressions in REGEXP
the entire match is collected.  In any case the searched buffer
is not modified."
  (interactive
   (nconc (occur-read-primary-args)
          (and (use-region-p) (list (region-bounds)))))
  (let* ((start (and (caar region) (max (caar region) (point-min))))
         (end (and (cdar region) (min (cdar region) (point-max))))
         (in-region (or start end))
         (bufs (if (not in-region) (list (current-buffer))
                 (let ((ol (make-overlay
                            (or start (point-min))
                            (or end (point-max)))))
                   (overlay-put ol 'occur--orig-point (point))
                   (list ol)))))
    (occur-1 regexp nlines bufs)))

(defvar ido-ignore-item-temp-list)

(defun multi-occur--prompt ()
  (concat
   "Next buffer to search "
   (cond
    ((or (eq read-buffer-function #'ido-read-buffer)
         (bound-and-true-p ido-everywhere))
     (substitute-command-keys
      "(\\<ido-completion-map>\\[ido-select-text] to end): "))
    ((bound-and-true-p fido-mode)
     (substitute-command-keys
      "(\\<icomplete-fido-mode-map>\\[icomplete-fido-exit] to end): "))
    (t "(RET to end): "))))

(defun multi-occur (bufs regexp &optional nlines)
  "Show all lines in buffers BUFS containing a match for REGEXP.
Optional argument NLINES specifies the number of context lines to show
with each match, see `list-matching-lines-default-context-lines'.
This function acts on multiple buffers; otherwise, it is exactly like
`occur'.  When you invoke this command interactively, you must specify
the buffer names that you want, one by one.
See also `multi-occur-in-matching-buffers'."
  (interactive
   (cons
    (let* ((bufs (list (read-buffer "First buffer to search: "
				    (current-buffer) t)))
	   (buf nil)
	   (ido-ignore-item-temp-list bufs))
      (while (not (string-equal
		   (setq buf (read-buffer (multi-occur--prompt) nil t))
		   ""))
	(cl-pushnew buf bufs)
	(setq ido-ignore-item-temp-list bufs))
      (nreverse (mapcar #'get-buffer bufs)))
    (occur-read-primary-args)))
  (occur-1 regexp nlines bufs))

(defun multi-occur-in-matching-buffers (bufregexp regexp &optional allbufs)
  "Show all lines matching REGEXP in buffers specified by BUFREGEXP.
Normally BUFREGEXP matches against each buffer's visited file name,
but ALLBUFS non-nil (interactively, if you specify a prefix argument),
it matches against the buffer name and includes also buffers that
don't visit files.
See also `multi-occur'."
  (interactive
   (cons
    (let* ((default (car regexp-history))
	   (input
	    (read-regexp
	     (if current-prefix-arg
		 "List lines in buffers whose names match regexp: "
	       "List lines in buffers whose filenames match regexp: "))))
      (if (equal input "")
	  default
	input))
    (occur-read-primary-args)))
  (when bufregexp
    (occur-1 regexp nil
	     (delq nil
		   (mapcar (lambda (buf)
			     (when (if allbufs
				       (string-match bufregexp
						     (buffer-name buf))
				     (and (buffer-file-name buf)
					  (string-match bufregexp
							(buffer-file-name buf))))
			       buf))
			   (buffer-list))))))

(defun occur-regexp-descr (regexp)
  (format " for %s\"%s\""
          (or (get-text-property 0 'isearch-regexp-function-descr regexp)
              "")
          (if (get-text-property 0 'isearch-string regexp)
              (propertize
               (query-replace-descr
                (get-text-property 0 'isearch-string regexp))
               'help-echo regexp)
            (query-replace-descr regexp))))

(defun occur-1 (regexp nlines bufs &optional buf-name)
  ;; BUFS is a list of buffer-or-overlay!
  (unless (and regexp (not (equal regexp "")))
    (error "Occur doesn't work with the empty regexp"))
  (unless buf-name
    (setq buf-name "*Occur*"))
  (let (occur-buf
	(active-bufs
         (delq nil (mapcar (lambda (boo)
			       (when (or (buffer-live-p boo)
                                         (and (overlayp boo)
                                              (overlay-buffer boo)))
                                 boo))
                           bufs)))
        (source-buffer-default-directory default-directory))
    ;; Handle the case where one of the buffers we're searching is the
    ;; output buffer.  Just rename it.
    (when (member buf-name
                  ;; FIXME: Use cl-exists.
                  (mapcar
                   (lambda (boo)
                     (buffer-name (if (overlayp boo) (overlay-buffer boo) boo)))
                   active-bufs))
      (with-current-buffer buf-name
	(rename-uniquely)))

    ;; Now find or create the output buffer.
    ;; If we just renamed that buffer, we will make a new one here.
    (setq occur-buf (get-buffer-create buf-name))

    (with-current-buffer occur-buf
      ;; Make the default-directory of the *Occur* buffer match that of
      ;; the buffer where the occurrences come from
      (setq default-directory source-buffer-default-directory)
      (setq overlay-arrow-position nil)
      (if (stringp nlines)
	  (fundamental-mode) ;; This is for collect operation.
	(occur-mode))
      (let ((inhibit-read-only t)
	    ;; Don't generate undo entries for creation of the initial contents.
	    (buffer-undo-list t)
	    (occur--final-pos nil))
	(erase-buffer)
	(let ((count
	       (if (stringp nlines)
                   ;; Treat nlines as a regexp to collect.
		   (let ((count 0))
		     (dolist (boo active-bufs)
		       (with-current-buffer
                           (if (overlayp boo) (overlay-buffer boo) boo)
			 (save-excursion
			   (goto-char
                            (if (overlayp boo) (overlay-start boo) (point-min)))
                           (let ((end (if (overlayp boo) (overlay-end boo))))
			     (while (re-search-forward regexp end t)
                               ;; Insert the replacement regexp.
			       (let ((str (match-substitute-replacement
                                           nlines)))
			         (if str
				     (with-current-buffer occur-buf
				       (insert str)
				       (setq count (1+ count))
				       (or (zerop (current-column))
					   (insert "\n"))))))))))
                     count)
		 ;; Perform normal occur.
		 (occur-engine
		  regexp active-bufs occur-buf
		  (or nlines list-matching-lines-default-context-lines)
		  (if (and case-fold-search search-upper-case)
		      (isearch-no-upper-case-p regexp t)
		    case-fold-search)
		  list-matching-lines-buffer-name-face
		  (if (face-differs-from-default-p list-matching-lines-prefix-face)
		      list-matching-lines-prefix-face)
		  list-matching-lines-face
		  (not (eq occur-excluded-properties t))))))
	  (let* ((bufcount (length active-bufs))
		 (diff (- (length bufs) bufcount)))
	    (message "Searched %d %s%s; %s %s%s"
		     bufcount
		     (ngettext "buffer" "buffers" bufcount)
		     (if (zerop diff) "" (format " (%d killed)" diff))
		     (if (zerop count) "no" (format "%d" count))
		     (ngettext "match" "matches" count)
		     ;; Don't display regexp if with remaining text
		     ;; it is longer than window-width.
		     (if (> (+ (length (or (get-text-property 0 'isearch-string regexp)
					   regexp))
			       42)
			    (window-width))
			 "" (occur-regexp-descr regexp))))
          (unless (eq bufs (nth 2 occur-revert-arguments))
            (occur--garbage-collect-revert-args))
	  (setq occur-revert-arguments (list regexp nlines bufs))
          (if (= count 0)
              (kill-buffer occur-buf)
            (display-buffer occur-buf)
            (when occur--final-pos
              (set-window-point
               (get-buffer-window occur-buf 'all-frames)
               occur--final-pos))
            (setq next-error-last-buffer occur-buf)
            (setq buffer-read-only t)
            (set-buffer-modified-p nil)
            (run-hooks 'occur-hook)))))))

(defun occur-engine (regexp buffers out-buf nlines case-fold
			    title-face prefix-face match-face keep-props)
  ;; BUFFERS is a list of buffer-or-overlay!
  (with-current-buffer out-buf
    (let ((global-lines 0)    ;; total count of matching lines
	  (global-matches 0)  ;; total count of matches
	  (coding nil)
	  (case-fold-search case-fold)
	  (multi-occur-p (cdr buffers)))
      ;; Map over all the buffers
      (dolist (boo buffers)
	(when (if (overlayp boo) (overlay-buffer boo) (buffer-live-p boo))
	  (with-current-buffer (if (overlayp boo) (overlay-buffer boo) boo)
            (let ((inhibit-field-text-motion t)
                  (lines 0)               ; count of matching lines
	          (matches 0)             ; count of matches
		  (headerpt (with-current-buffer out-buf (point)))
		  (orig-line (if (not (overlayp boo))
				 (line-number-at-pos)
			       (line-number-at-pos
				(overlay-get boo 'occur--orig-point)))))
	      (save-excursion
                ;; begin searching in the buffer
		(goto-char (if (overlayp boo) (overlay-start boo) (point-min)))
                (forward-line 0)
	        (let* ((limit (if (overlayp boo) (overlay-end boo) (point-max)))
                       (start-line (line-number-at-pos))
		       (curr-line start-line) ; line count
		       (orig-line-shown-p)
		       (prev-line nil)        ; line number of prev match endpt
		       (prev-after-lines nil) ; context lines of prev match
		       (matchbeg 0)
		       (origpt nil)
		       (begpt nil)
		       (endpt nil)
                       markers            ; list of (BEG-MARKER . END-MARKER)
		       (curstring "")
		       (ret nil)
	               ;; The following binding is for when case-fold-search
	               ;; has a local binding in the original buffer, in which
	               ;; case we cannot bind it globally and let that have
	               ;; effect in every buffer we search.
                       (case-fold-search case-fold))
	          (or coding
		      ;; Set CODING only if the current buffer locally
		      ;; binds buffer-file-coding-system.
		      (not (local-variable-p 'buffer-file-coding-system))
		      (setq coding buffer-file-coding-system))
		  (while (< (point) limit)
		    (setq origpt (point))
		    (when (setq endpt (re-search-forward regexp limit t))
		      (setq lines (1+ lines)) ;; increment matching lines count
		      (setq matchbeg (match-beginning 0))
		      ;; Get beginning of first match line and end of the last.
		      (save-excursion
		        (goto-char matchbeg)
		        (setq begpt (line-beginning-position))
		        (goto-char endpt)
		        (setq endpt (line-end-position)))
		      ;; Sum line numbers up to the first match line.
		      (setq curr-line (+ curr-line (count-lines origpt begpt)))
                      (setq markers nil)
		      (setq curstring (occur-engine-line begpt endpt keep-props))
		      ;; Highlight the matches
		      (let ((len (length curstring))
			    (start 0))
		        ;; Count empty lines that don't use next loop (Bug#22062).
		        (when (zerop len)
			  (setq matches (1+ matches)))
			(when (and list-matching-lines-jump-to-current-line
				   (not multi-occur-p))
			  (or orig-line (setq orig-line 1))
			  (or nlines (setq nlines (line-number-at-pos (point-max))))
			  (when (= curr-line orig-line)
			    (add-face-text-property
			     0 len list-matching-lines-current-line-face nil curstring)
			    (add-text-properties 0 len '(current-line t) curstring))
			  (when (and (>= orig-line (- curr-line nlines))
				     (<= orig-line (+ curr-line nlines)))
			    ;; Shown either here or will be shown by occur-context-lines
			    (setq orig-line-shown-p t)))
		        (while (and (< start len)
				    (string-match regexp curstring start))
                          (push (cons (set-marker (make-marker)
                                                  (+ begpt (match-beginning 0)))
                                      (set-marker (make-marker)
                                                  (+ begpt (match-end 0))))
                                markers)
			  (setq matches (1+ matches))
			  (add-text-properties
			   (match-beginning 0) (match-end 0)
			   '(occur-match t) curstring)
			  (when match-face
			    ;; Add `match-face' to faces copied from the buffer.
			    (add-face-text-property
			     (match-beginning 0) (match-end 0)
			     match-face nil curstring))
			  ;; Avoid infloop (Bug#7593).
			  (let ((end (match-end 0)))
			    (setq start (if (= start end) (1+ start) end)))))
                      (setq markers (nreverse markers))
		      ;; Generate the string to insert for this match
		      (let* ((match-prefix
			      ;; Using 7 digits aligns tabs properly.
			      (apply #'propertize (format "%7d:" curr-line)
				     (append
				      (when prefix-face
				        `(font-lock-face ,prefix-face))
				      `(occur-prefix t
				                     ;; Allow insertion of text
				                     ;; at the end of the prefix
				                     ;; (for Occur Edit mode).
				                     front-sticky t
						     rear-nonsticky t
                                                     read-only t
						     occur-target ,markers
						     follow-link t
				                     help-echo "mouse-2: go to this occurrence"))))
			     (match-str
			      ;; We don't put `mouse-face' on the newline,
			      ;; because that loses.  And don't put it
			      ;; on context lines to reduce flicker.
			      (propertize curstring
					  'occur-target markers
					  'follow-link t
					  'help-echo
					  "mouse-2: go to this occurrence"))
			     (out-line
			      ;; Add non-numeric prefix to all non-first lines
			      ;; of multi-line matches.
                              (concat
			       (string-replace
			        "\n"
			        (if prefix-face
				    (propertize
				     "\n       :" 'font-lock-face prefix-face
                                     'occur-target markers)
                                  (propertize
				   "\n       :" 'occur-target markers))
                                ;; Add mouse face in one section to
                                ;; ensure the prefix and the string
                                ;; get a contiguous highlight.
			        (propertize (concat match-prefix match-str)
                                            'mouse-face 'highlight))
			       ;; Add markers at eol, but no mouse props.
			       (propertize "\n" 'occur-target markers)))
			     (data
			      (if (= nlines 0)
				  ;; The simple display style
				  out-line
			        ;; The complex multi-line display style.
			        (setq ret (occur-context-lines
					   out-line nlines keep-props begpt
					   endpt curr-line prev-line
					   prev-after-lines prefix-face
					   orig-line multi-occur-p))
			        ;; Set first elem of the returned list to `data',
			        ;; and the second elem to `prev-after-lines'.
			        (setq prev-after-lines (nth 1 ret))
			        (nth 0 ret)))
			     (orig-line-str
			      (when (and list-matching-lines-jump-to-current-line
					 (null orig-line-shown-p)
					 (> curr-line orig-line))
				(setq orig-line-shown-p t)
				(save-excursion
				  (goto-char (point-min))
				  (forward-line (1- orig-line))
				  (occur-engine-line (line-beginning-position)
						     (line-end-position) keep-props)))))
		        ;; Actually insert the match display data
		        (with-current-buffer out-buf
			  (when orig-line-str
			    (add-face-text-property
			     0 (length orig-line-str)
			     list-matching-lines-current-line-face nil orig-line-str)
			    (add-text-properties 0 (length orig-line-str)
						 '(current-line t) orig-line-str)
			    (insert (car (occur-engine-add-prefix
					  (list orig-line-str) prefix-face))))
			  (insert data)))
		      (goto-char endpt))
		    (if endpt
		        (progn
			  ;; Sum line numbers between first and last match lines.
			  (setq curr-line (+ curr-line (count-lines begpt endpt)
					     ;; Add 1 for empty last match line
					     ;; since count-lines returns one
					     ;; line less.
					     (if (and (bolp) (eolp)) 1 0)))
			  ;; On to the next match...
			  (forward-line 1))
		      (goto-char (point-max)))
		    (setq prev-line (1- curr-line)))
		  ;; Flush remaining context after-lines.
		  (when prev-after-lines
		    (with-current-buffer out-buf
		      (insert (apply #'concat (occur-engine-add-prefix
					       prev-after-lines prefix-face)))))
		  (when (and list-matching-lines-jump-to-current-line
			     (null orig-line-shown-p))
		    (setq orig-line-shown-p t)
		    (let ((orig-line-str
			   (save-excursion
			     (goto-char (point-min))
			     (forward-line (1- orig-line))
			     (occur-engine-line (line-beginning-position)
						(line-end-position) keep-props))))
		      (add-face-text-property
		       0 (length orig-line-str)
		       list-matching-lines-current-line-face nil orig-line-str)
		      (add-text-properties 0 (length orig-line-str)
					   '(current-line t) orig-line-str)
		      (with-current-buffer out-buf
			(insert (car (occur-engine-add-prefix
			              (list orig-line-str) prefix-face))))))))
	      (when (not (zerop lines)) ;; is the count zero?
	        (setq global-lines (+ global-lines lines)
		      global-matches (+ global-matches matches))
	        (with-current-buffer out-buf
		  (goto-char headerpt)
		  (let ((beg (point))
		        end)
		    (insert (propertize
			     (format "%d %s%s%s in buffer: %s%s\n"
				     matches
				     (ngettext "match" "matches" matches)
				     ;; Don't display the same number of lines
				     ;; and matches in case of 1 match per line.
				     (if (= lines matches)
				         "" (format " in %d %s"
						    lines
						    (ngettext "line" "lines" lines)))
				     ;; Don't display regexp for multi-buffer.
				     (if (> (length buffers) 1)
				         "" (occur-regexp-descr regexp))
				     (buffer-name (if (overlayp boo) (overlay-buffer boo) boo))
				     (if (overlayp boo)
					 (format " within region: %d-%d"
						 (overlay-start boo)
						 (overlay-end boo))
				       ""))
			     'read-only t))
		    (setq end (point))
		    (when title-face
		      (add-face-text-property beg end title-face))
		    (goto-char (if (and list-matching-lines-jump-to-current-line
					(not multi-occur-p))
				   (setq occur--final-pos
					 (and (goto-char (point-max))
					      (or (previous-single-property-change (point) 'current-line)
						  (point-max))))
				 (point-min))))))))))
      ;; Display total match count and regexp for multi-buffer.
      (when (and (not (zerop global-lines)) (> (length buffers) 1))
	(goto-char (point-min))
	(let ((beg (point))
	      end)
	  (insert (format "%d %s%s total%s:\n"
			  global-matches
			  (ngettext "match" "matches" global-matches)
			  ;; Don't display the same number of lines
			  ;; and matches in case of 1 match per line.
			  (if (= global-lines global-matches)
			      "" (format " in %d %s"
					 global-lines
					 (ngettext "line" "lines" global-lines)))
			  (occur-regexp-descr regexp)))
	  (setq end (point))
	  (when title-face
	    (add-face-text-property beg end title-face)))
	(goto-char (point-min)))
      (if coding
	  ;; CODING is buffer-file-coding-system of the first buffer
	  ;; that locally binds it.  Let's use it also for the output
	  ;; buffer.
	  (set-buffer-file-coding-system coding))
      ;; Return the number of matches
      global-matches)))

(defun occur-engine-line (beg end &optional keep-props)
  (if (and keep-props font-lock-mode)
      (font-lock-ensure beg end))
  (if (and keep-props (not (eq occur-excluded-properties t)))
      (let ((str (buffer-substring beg end)))
	(remove-list-of-text-properties
	 0 (length str) occur-excluded-properties str)
	str)
    (buffer-substring-no-properties beg end)))

(defun occur-engine-add-prefix (lines &optional prefix-face)
  (mapcar
   (lambda (line)
     (concat (if prefix-face
                 (propertize "       :" 'font-lock-face prefix-face)
               "       :")
             line "\n"))
   lines))

(defun occur-accumulate-lines (count &optional keep-props pt)
  (save-excursion
    (when pt
      (goto-char pt))
    (let ((forwardp (> count 0))
	  result beg end moved)
      (while (not (or (zerop count)
		      (if forwardp
			  (eobp)
			(and (bobp) (not moved)))))
	(setq count (+ count (if forwardp -1 1)))
	(setq beg (line-beginning-position)
	      end (line-end-position))
	(push (occur-engine-line beg end keep-props) result)
	(setq moved (= 0 (forward-line (if forwardp 1 -1)))))
      (nreverse result))))

;; Generate context display for occur.
;; OUT-LINE is the line where the match is.
;; NLINES and KEEP-PROPS are args to occur-engine.
;; CURR-LINE is line count of the current match,
;; PREV-LINE is line count of the previous match,
;; PREV-AFTER-LINES is a list of after-context lines of the previous match.
;; Generate a list of lines, add prefixes to all but OUT-LINE,
;; then concatenate them all together.
(defun occur-context-lines (out-line nlines keep-props begpt endpt
				     curr-line prev-line prev-after-lines
				     &optional prefix-face
				     orig-line multi-occur-p)
  ;; Find after- and before-context lines of the current match.
  (let ((before-lines
	 (nreverse (cdr (occur-accumulate-lines
			 (- (1+ (abs nlines))) keep-props begpt))))
	(after-lines
	 (cdr (occur-accumulate-lines
	       (1+ nlines) keep-props endpt)))
	separator)

    (when (and list-matching-lines-jump-to-current-line
	       (not multi-occur-p))
      (when (and (>= orig-line (- curr-line nlines))
		 (< orig-line curr-line))
	(let ((curstring (nth (- (length before-lines) (- curr-line orig-line)) before-lines)))
	  (add-face-text-property
	   0 (length curstring)
	   list-matching-lines-current-line-face nil curstring)
	  (add-text-properties 0 (length curstring)
			       '(current-line t) curstring)))
      (when (and (<= orig-line (+ curr-line nlines))
		 (> orig-line curr-line))
	(let ((curstring (nth (- orig-line curr-line 1) after-lines)))
	  (add-face-text-property
	   0 (length curstring)
	   list-matching-lines-current-line-face nil curstring)
	  (add-text-properties 0 (length curstring)
			       '(current-line t) curstring))))

    ;; Combine after-lines of the previous match
    ;; with before-lines of the current match.

    (when prev-after-lines
      ;; Don't overlap prev after-lines with current before-lines.
      (if (>= (+ prev-line (length prev-after-lines))
	      (- curr-line (length before-lines)))
	  (setq prev-after-lines
                (take (- curr-line prev-line (length before-lines) 1)
                      prev-after-lines))
	;; Separate non-overlapping context lines with a dashed line.
	(setq separator "-------\n")))

    (when prev-line
      ;; Don't overlap current before-lines with previous match line.
      (if (<= (- curr-line (length before-lines))
	      prev-line)
	  (setq before-lines
		(nthcdr (- (length before-lines)
			   (- curr-line prev-line 1))
			before-lines))
	;; Separate non-overlapping before-context lines.
	(unless (> nlines 0)
	  (setq separator "-------\n"))))

    (list
     ;; Return a list where the first element is the output line.
     (apply #'concat
	    (append
	     (if prev-after-lines
		 (occur-engine-add-prefix prev-after-lines prefix-face))
	     (if separator
		 (list (if prefix-face
			   (propertize separator 'font-lock-face prefix-face)
			 separator)))
	     (occur-engine-add-prefix before-lines prefix-face)
	     (list out-line)))
     ;; And the second element is the list of context after-lines.
     (if (> nlines 0) after-lines))))

(defun occur-word-at-mouse (event)
  "Display an occur buffer for the word at EVENT."
  (interactive "e")
  (let ((word (thing-at-mouse event 'word t)))
    (occur (concat "\\<" (regexp-quote word) "\\>"))))

(defun occur-symbol-at-mouse (event)
  "Display an occur buffer for the symbol at EVENT."
  (interactive "e")
  (let ((symbol (thing-at-mouse event 'symbol t)))
    (occur (concat "\\_<" (regexp-quote symbol) "\\_>"))))

(defun occur-context-menu (menu click)
  "Populate MENU with occur commands at CLICK.
To be added to `context-menu-functions'."
  (let ((word (thing-at-mouse click 'word))
        (sym (thing-at-mouse click 'symbol)))
    (when (or word sym)
      (define-key-after menu [occur-separator] menu-bar-separator
        'middle-separator)
      (when sym
        (define-key-after menu [occur-symbol-at-mouse]
          '(menu-item "Occur Symbol" occur-symbol-at-mouse)
          'occur-separator))
      (when word
        (define-key-after menu [occur-word-at-mouse]
          '(menu-item "Occur Word" occur-word-at-mouse)
          'occur-separator))))
  menu)


;; It would be nice to use \\[...], but there is no reasonable way
;; to make that display both SPC and Y.
(defconst query-replace-help
  "Type \\`SPC' or \\`y' to replace one match, Delete or \\`n' to skip to next,
\\`RET' or \\`q' to exit, Period to replace one match and exit,
\\`,' to replace but not move point immediately,
\\`!' to replace all remaining matches in this buffer with no more questions,
\\`C-r' to enter recursive edit (\\[exit-recursive-edit] to get out again),
\\`C-w' to delete match and then enter recursive edit,
\\`^' to move point back to previous match,
\\`u' to undo previous replacement,
\\`U' to undo all replacements,
\\`e' to edit the replacement string.
\\`E' to edit the replacement string with exact case.
\\`C-l' to clear the screen, redisplay, and offer same replacement again,
\\`Y' to replace all remaining matches in all remaining buffers (in
multi-buffer replacements) with no more questions,
\\`N' (in multi-buffer replacements) to skip to the next buffer without
replacing remaining matches in the current buffer.
Any other character exits the interactive replacement loop, and is then
re-executed as a normal key sequence."
  "Help message while in `query-replace'.")

(defvar query-replace-map
  (let ((map (make-sparse-keymap)))
    (define-key map " " 'act)
    (define-key map "\d" 'skip)
    (define-key map [delete] 'skip)
    (define-key map [backspace] 'skip)
    (define-key map "y" 'act)
    (define-key map "n" 'skip)
    (define-key map "Y" 'act)
    (define-key map "N" 'skip)
    (define-key map "e" 'edit-replacement)
    (define-key map "E" 'edit-replacement-exact-case)
    (define-key map "," 'act-and-show)
    (define-key map "q" 'exit)
    (define-key map "\r" 'exit)
    (define-key map [return] 'exit)
    (define-key map "." 'act-and-exit)
    (define-key map "\C-r" 'edit)
    (define-key map "\C-w" 'delete-and-edit)
    (define-key map "\C-l" 'recenter)
    (define-key map "!" 'automatic)
    (define-key map "^" 'backup)
    (define-key map "u" 'undo)
    (define-key map "U" 'undo-all)
    (define-key map "\C-h" 'help)
    (define-key map [f1] 'help)
    (define-key map [help] 'help)
    (define-key map "?" 'help)
    (define-key map "\C-g" 'quit)
    (define-key map "\C-]" 'quit)
    (define-key map "\C-v" 'scroll-up)
    (define-key map "\M-v" 'scroll-down)
    (define-key map [next] 'scroll-up)
    (define-key map [prior] 'scroll-down)
    (define-key map [?\C-\M-v] 'scroll-other-window)
    (define-key map [M-next] 'scroll-other-window)
    (define-key map [?\C-\M-\S-v] 'scroll-other-window-down)
    (define-key map [M-prior] 'scroll-other-window-down)
    ;; Binding ESC would prohibit the M-v binding.  Instead, callers
    ;; should check for ESC specially.
    ;; (define-key map "\e" 'exit-prefix)
    (define-key map [escape] 'exit-prefix)
    map)
  "Keymap of responses to questions posed by commands like `query-replace'.
The \"bindings\" in this map are not commands; they are answers.
The valid answers include `act', `skip', `act-and-show',
`act-and-exit', `exit', `exit-prefix', `recenter', `scroll-up',
`scroll-down', `scroll-other-window', `scroll-other-window-down',
`edit', `edit-replacement', `edit-replacement-exact-case',
`delete-and-edit', `automatic', `backup', `undo', `undo-all',
`quit', and `help'.

This keymap is used by `y-or-n-p' as well as `query-replace'.")

(defvar-keymap multi-query-replace-map
  :doc "Keymap that defines additional bindings for multi-buffer replacements.
It extends its parent map `query-replace-map' with new bindings to
operate on a set of buffers/files.  The difference with its parent map
is the additional answers `automatic-all' to replace all remaining
matches in all remaining buffers with no more questions, and
`exit-current' to skip remaining matches in the current buffer
and to continue with the next buffer in the sequence."
  :parent query-replace-map
  "Y" 'automatic-all
  "N" 'exit-current)

(defun replace-match-string-symbols (n)
  "Process a list (and any sub-lists), expanding certain symbols.
Symbol  Expands To
N     (match-string N)           (where N is a string of digits)
#N    (string-to-number (match-string N))
&     (match-string 0)
#&    (string-to-number (match-string 0))
#     replace-count

Note that these symbols must be preceded by a backslash in order to
type them using Lisp syntax."
  (while (consp n)
    (cond
     ((consp (car n))
      (replace-match-string-symbols (car n))) ;Process sub-list
     ((symbolp (car n))
      (let ((name (symbol-name (car n))))
        (cond
         ((string-match "^[0-9]+$" name)
          (setcar n (list 'match-string (string-to-number name))))
         ((string-match "^#[0-9]+$" name)
          (setcar n (list 'string-to-number
                          (list 'match-string
                                (string-to-number (substring name 1))))))
         ((string= "&" name)
          (setcar n '(match-string 0)))
         ((string= "#&" name)
          (setcar n '(string-to-number (match-string 0))))
	 ((string= "#" name)
	  (setcar n 'replace-count))))))
    (setq n (cdr n))))

(defun replace-eval-replacement (expression count)
  (let* ((replace-count count)
         (replacement
          (condition-case err
              (eval expression)
            (error
             (error "Error evaluating replacement expression: %S" err)))))
    (if (stringp replacement)
        replacement
      (prin1-to-string replacement t))))

(defun replace-quote (replacement)
  "Quote a replacement string.
This just doubles all backslashes in REPLACEMENT and
returns the resulting string.  If REPLACEMENT is not
a string, it is first passed through `prin1-to-string'
with the `noescape' argument set.

`match-data' is preserved across the call."
  (string-replace "\\" "\\\\"
		  (if (stringp replacement)
		      replacement
		    (prin1-to-string replacement t))))

(defun replace-loop-through-replacements (data count)
  ;; DATA is a vector containing the following values:
  ;;   0 next-rotate-count
  ;;   1 repeat-count
  ;;   2 next-replacement
  ;;   3 replacements
  (if (= (aref data 0) count)
      (progn
        (aset data 0 (+ count (aref data 1)))
        (let ((next (cdr (aref data 2))))
          (aset data 2 (if (consp next) next (aref data 3))))))
  (car (aref data 2)))

(defun replace-match-data (integers reuse &optional new)
  "Like `match-data', but markers in REUSE get invalidated.
If NEW is non-nil, it is set and returned instead of fresh data,
but coerced to the correct value of INTEGERS."
  (or (and new
	   (progn
	     (set-match-data new)
	     (and (eq new reuse)
		  (eq (null integers) (markerp (car reuse)))
		  new)))
      (match-data integers reuse t)))

(defun replace-match-maybe-edit (newtext fixedcase literal noedit match-data
                                 &optional backward)
  "Make a replacement with `replace-match', editing `\\?'.
FIXEDCASE, LITERAL are passed to `replace-match' (which see).
After possibly editing it (if `\\?' is present), NEWTEXT is also
passed to `replace-match'.  If NOEDIT is true, no check for `\\?'
is made (to save time).
MATCH-DATA is used for the replacement, and is a data structure
as returned from the `match-data' function.
In case editing is done, it is changed to use markers.  BACKWARD is
used to reverse the replacement direction.

The return value is non-nil if there has been no `\\?' or NOEDIT was
passed in.  If LITERAL is set, no checking is done, anyway."
  (unless (or literal noedit)
    (setq noedit t)
    (while (string-match "\\(\\`\\|[^\\]\\)\\(\\\\\\\\\\)*\\(\\\\\\?\\)"
			 newtext)
      (setq newtext
	    (read-string "Edit replacement string: "
                         (prog1
                             (cons
                              (replace-match "" t t newtext 3)
                              (1+ (match-beginning 3)))
                           (setq match-data
                                 (replace-match-data
                                  nil match-data match-data))))
	    noedit nil)))
  (set-match-data match-data)
  (replace-match newtext fixedcase literal)
  ;; `replace-match' leaves point at the end of the replacement text,
  ;; so move point to the beginning when replacing backward.
  (when backward (goto-char (nth 0 match-data)))
  noedit)

(defvar replace-update-post-hook nil
  "Function(s) to call after `query-replace' has found a match in the buffer.")

(defvar replace-search-function nil
  "Function to use when searching for strings to replace.
It is used by `query-replace' and `replace-string', and is called
with three arguments, as if it were `search-forward'.")

(defvar replace-re-search-function nil
  "Function to use when searching for regexps to replace.
It is used by `query-replace-regexp', `replace-regexp', and
`map-query-replace-regexp'.  It is called with three arguments,
as if it were `re-search-forward'.")

(defvar replace-regexp-function nil
  "Function to convert the FROM string of query-replace commands to a regexp.
This is used by `query-replace', `query-replace-regexp', etc. as
the value of `isearch-regexp-function' when they search for the
occurrences of the string/regexp to be replaced.  This is intended
to be used when the string to be replaced, as typed by the user,
is not to be interpreted literally, but instead should be converted
to a regexp that is actually used for the search.")

(defun replace-search (search-string limit regexp-flag delimited-flag
		       case-fold &optional backward)
  "Search for the next occurrence of SEARCH-STRING to replace."
  ;; Let-bind global isearch-* variables to values used
  ;; to search the next replacement.  These let-bindings
  ;; should be effective both at the time of calling
  ;; `isearch-search-fun-default' and also at the
  ;; time of funcalling `search-function'.
  ;; These isearch-* bindings can't be placed higher
  ;; outside of this function because then another I-search
  ;; used after `recursive-edit' might override them.
  (let* ((isearch-regexp regexp-flag)
	 (isearch-regexp-function (or replace-regexp-function
				      delimited-flag
				      (and replace-char-fold
					   (not regexp-flag)
					   #'char-fold-to-regexp)))
	 (isearch-lax-whitespace
	  replace-lax-whitespace)
	 (isearch-regexp-lax-whitespace
	  replace-regexp-lax-whitespace)
	 (isearch-case-fold-search case-fold)
	 (isearch-adjusted nil)
	 (isearch-nonincremental t)	; don't use lax word mode
	 (isearch-forward (not backward))
	 (search-function
	  (or (if regexp-flag
		  replace-re-search-function
		replace-search-function)
              ;; `isearch-search-fun' can't be used here because
              ;; when buffer-local `isearch-search-fun-function'
              ;; searches e.g. the minibuffer history, then
              ;; `query-replace' should not operate on the whole
              ;; history, but only on the minibuffer contents.
	      (isearch-search-fun-default))))
    (funcall search-function search-string limit t)))

(defvar replace-overlay nil)
(defvar replace-submatches-overlays nil)

(defun replace-highlight (match-beg match-end range-beg range-end
			  search-string regexp-flag delimited-flag
			  case-fold &optional backward)
  (if query-replace-highlight
      (if replace-overlay
	  (move-overlay replace-overlay match-beg match-end (current-buffer))
	(setq replace-overlay (make-overlay match-beg match-end))
	(overlay-put replace-overlay 'priority 1001) ;higher than lazy overlays
	(overlay-put replace-overlay 'face 'query-replace)))

  (when (and query-replace-highlight-submatches regexp-flag)
    (mapc 'delete-overlay replace-submatches-overlays)
    (setq replace-submatches-overlays nil)
    ;; 'cddr' removes whole expression match from match-data
    (let ((submatch-data (cddr (match-data t)))
          (group 0)
          b e ov face)
      (while submatch-data
        (setq b (pop submatch-data)
              e (pop submatch-data))
        (when (and (integer-or-marker-p b)
                   (integer-or-marker-p e))
          (setq ov (make-overlay b e)
                group (1+ group)
                face (intern-soft (format "isearch-group-%d" group)))
          ;; Recycle faces from beginning
          (unless (facep face)
            (setq group 1 face 'isearch-group-1))
          (overlay-put ov 'face face)
          (overlay-put ov 'priority 1002)
          (push ov replace-submatches-overlays)))))

  (if query-replace-lazy-highlight
      (let ((isearch-string search-string)
	    (isearch-regexp regexp-flag)
	    (isearch-regexp-function (or replace-regexp-function
					 delimited-flag
					 (and replace-char-fold
					      (not regexp-flag)
					      #'char-fold-to-regexp)))
	    (isearch-lax-whitespace
	     replace-lax-whitespace)
	    (isearch-regexp-lax-whitespace
	     replace-regexp-lax-whitespace)
	    (isearch-case-fold-search case-fold)
	    (isearch-invisible search-invisible)
	    (isearch-forward (not backward))
	    (isearch-other-end match-beg)
	    (isearch-error nil)
	    (isearch-lazy-count nil)
	    (lazy-highlight-buffer nil))
	(isearch-lazy-highlight-new-loop range-beg range-end))))

(defun replace-dehighlight ()
  (when replace-overlay
    (delete-overlay replace-overlay))
  (when query-replace-highlight-submatches
    (mapc 'delete-overlay replace-submatches-overlays)
    (setq replace-submatches-overlays nil))
  (when query-replace-lazy-highlight
    (lazy-highlight-cleanup lazy-highlight-cleanup)
    (setq isearch-lazy-highlight-last-string nil))
  ;; Close overlays opened by `isearch-range-invisible' in `perform-replace'.
  (isearch-clean-overlays))

;; A macro because we push STACK, i.e. a local var in `perform-replace'.
(defmacro replace--push-stack (replaced search-str next-replace stack)
  (declare (indent 0) (debug (form form form gv-place)))
  `(push (list (point) ,replaced
;;;  If the replacement has already happened, all we need is the
;;;  current match start and end.  We could get this with a trivial
;;;  match like
;;;  (save-excursion (goto-char (match-beginning 0))
;;;		     (search-forward (match-string 0))
;;;                  (match-data t))
;;;  if we really wanted to avoid manually constructing match data.
;;;  Adding current-buffer is necessary so that match-data calls can
;;;  return markers which are appropriate for editing.
	       (if ,replaced
		   (list
		    (match-beginning 0) (match-end 0) (current-buffer))
	         (match-data t))
	       ,search-str ,next-replace)
         ,stack))

(defun replace--region-filter (bounds)
  "Return a function that decides if a region is inside BOUNDS.
BOUNDS is a list of cons cells of the form (START . END).  The
returned function takes as argument two buffer positions, START
and END."
  (let ((region-bounds
         (mapcar (lambda (position)
                   (cons (copy-marker (car position))
                         (copy-marker (cdr position))))
                 bounds)))
    (lambda (start end)
      (delq nil (mapcar
                 (lambda (bounds)
                   (and
                    (>= start (car bounds))
                    (<= start (cdr bounds))
                    (>= end   (car bounds))
                    (<= end   (cdr bounds))))
                 region-bounds)))))

(defvar overriding-text-conversion-style)

(defun perform-replace (from-string replacements
		        query-flag regexp-flag delimited-flag
			&optional repeat-count map start end backward region-noncontiguous-p)
  "Subroutine of `query-replace'.  Its complexity handles interactive queries.
Don't use this in your own program unless you want to query and set the mark
just as `query-replace' does.  Instead, write a simple loop like this:

  (while (re-search-forward \"foo[ \\t]+bar\" nil t)
    (replace-match \"foobar\" nil nil))

which will run faster and probably do exactly what you want.  Please
see the documentation of `replace-match' to find out how to simulate
`case-replace'.

This function returns nil if there were no matches to make, or
the user canceled the call.

REPLACEMENTS is either a string, a list of strings, or a cons cell
containing a function and its first argument.  The function is
called to generate each replacement like this:
  (funcall (car replacements) (cdr replacements) replace-count)
It must return a string.

Non-nil REGION-NONCONTIGUOUS-P means that the region is composed of
noncontiguous pieces.  The most common example of this is a
rectangular region, where the pieces are separated by newline
characters."
  (or map (setq map query-replace-map))
  (and query-flag minibuffer-auto-raise
       (raise-frame (window-frame (minibuffer-window))))
  (let* ((case-fold-search
	  (if (and case-fold-search search-upper-case)
	      (isearch-no-upper-case-p from-string regexp-flag)
	    case-fold-search))
         (nocasify (not (and case-replace case-fold-search)))
         (literal (or (not regexp-flag) (eq regexp-flag 'literal)))
         (search-string from-string)
         (real-match-data nil)       ; The match data for the current match.
         (next-replacement nil)
         ;; This is non-nil if we know there is nothing for the user
         ;; to edit in the replacement.
         (noedit nil)
         (keep-going t)
         (stack nil)
         (search-string-replaced nil)    ; last string matching `from-string'
         (next-replacement-replaced nil) ; replacement string
                                         ; (substituted regexp)
         (last-was-undo)
         (last-was-act-and-show)
         (update-stack t)
         (replace-count 0)
         (skip-read-only-count 0)
         (skip-filtered-count 0)
         (skip-invisible-count 0)
         (nonempty-match nil)
	 (multi-buffer nil)
	 (recenter-last-op nil)	; Start cycling order with initial position.

         ;; If non-nil, it is marker saying where in the buffer to stop.
         (limit nil)
         (region-filter nil)

         ;; Disable text conversion during the replacement operation.
         (old-text-conversion-style (and (boundp 'overriding-text-conversion-style)
                                         overriding-text-conversion-style))
         overriding-text-conversion-style

         ;; Data for the next match.  If a cons, it has the same format as
         ;; (match-data); otherwise it is t if a match is possible at point.
         (match-again t)
         (message
          (if query-flag
              (apply #'propertize
                     (concat "Query replacing "
                             (if backward "backward " "")
                             (if delimited-flag
                                 (or (and (symbolp delimited-flag)
                                          (get delimited-flag
                                               'isearch-message-prefix))
                                     "word ") "")
                             (if regexp-flag "regexp " "")
                             "%s with %s: "
                             (substitute-command-keys
                              "(\\<query-replace-map>\\[help] for help) "))
                     minibuffer-prompt-properties))))

    ;; Unless a single contiguous chunk is selected, operate on multiple chunks.
    (when region-noncontiguous-p
      (setq region-filter (replace--region-filter
                           (funcall region-extract-function 'bounds)))
      (add-function :after-while isearch-filter-predicate region-filter))

    ;; If region is active, in Transient Mark mode, operate on region.
    (if backward
	(when end
	  (setq limit (copy-marker (min start end)))
	  (goto-char (max start end))
	  (deactivate-mark))
      (when start
	(setq limit (copy-marker (max start end)))
	(goto-char (min start end))
	(deactivate-mark)))

    ;; If last typed key in previous call of multi-buffer perform-replace
    ;; was `automatic-all', don't ask more questions in next files
    (when (eq (lookup-key map (vector last-input-event) t) 'automatic-all)
      (setq query-flag nil multi-buffer t))

    (cond
     ((stringp replacements)
      (setq next-replacement replacements
            replacements     nil))
     ((stringp (car replacements)) ; If it isn't a string, it must be a cons
      (or repeat-count (setq repeat-count 1))
      ;; This is a hand-made `iterator'.
      (setq replacements (cons #'replace-loop-through-replacements
                               (vector repeat-count repeat-count
                                       replacements replacements)))))

    (when query-replace-lazy-highlight
      (setq isearch-lazy-highlight-last-string nil))

    (push-mark)
    (undo-boundary)
    (when (and query-flag (fboundp 'set-text-conversion-style))
      (setq overriding-text-conversion-style nil)
      (set-text-conversion-style text-conversion-style))
    (unwind-protect
	;; Loop finding occurrences that perhaps should be replaced.
	(while (and keep-going
		    (if backward
			(not (or (bobp) (and limit (<= (point) limit))))
		      (not (or (eobp) (and limit (>= (point) limit)))))
		    ;; Use the next match if it is already known;
		    ;; otherwise, search for a match after moving forward
		    ;; one char if progress is required.
		    (setq real-match-data
			  (cond ((consp match-again)
				 (goto-char (if backward
						(nth 0 match-again)
					      (nth 1 match-again)))
				 (replace-match-data
				  t real-match-data match-again))
				;; MATCH-AGAIN non-nil means accept an
				;; adjacent match.
				(match-again
				 (and
				  (replace-search search-string limit
						  regexp-flag delimited-flag
						  case-fold-search backward)
				  ;; For speed, use only integers and
				  ;; reuse the list used last time.
				  (replace-match-data t real-match-data)))
				((and (if backward
					  (> (1- (point)) (point-min))
					(< (1+ (point)) (point-max)))
				      (or (null limit)
					  (if backward
					      (> (1- (point)) limit)
					    (< (1+ (point)) limit))))
				 ;; If not accepting adjacent matches,
				 ;; move one char to the right before
				 ;; searching again.  Undo the motion
				 ;; if the search fails.
				 (let ((opoint (point)))
				   (forward-char (if backward -1 1))
				   (if (replace-search search-string limit
						       regexp-flag delimited-flag
						       case-fold-search backward)
				       (replace-match-data
					t real-match-data)
				     (goto-char opoint)
				     nil))))))

	  ;; Record whether the match is nonempty, to avoid an infinite loop
	  ;; repeatedly matching the same empty string.
	  (setq nonempty-match
		(/= (nth 0 real-match-data) (nth 1 real-match-data)))

	  ;; If the match is empty, record that the next one can't be
	  ;; adjacent.

	  ;; Otherwise, if matching a regular expression, do the next
	  ;; match now, since the replacement for this match may
	  ;; affect whether the next match is adjacent to this one.
	  ;; If that match is empty, don't use it.
	  (setq match-again
		(and nonempty-match
		     (or (not regexp-flag)
			 (and (if backward
				  (looking-back search-string nil)
				(looking-at search-string))
			      (let ((match (match-data)))
				(and (/= (nth 0 match) (nth 1 match))
				     match))))))

	  (cond
	   ;; Optionally ignore matches that have a read-only property.
	   ((not (or (not query-replace-skip-read-only)
		     (not (text-property-not-all
			   (nth 0 real-match-data) (nth 1 real-match-data)
			   'read-only nil))))
	    (setq skip-read-only-count (1+ skip-read-only-count)))
	   ;; Optionally filter out matches.
	   ((not (funcall isearch-filter-predicate
                          (nth 0 real-match-data) (nth 1 real-match-data)))
	    (setq skip-filtered-count (1+ skip-filtered-count)))
	   ;; Optionally ignore invisible matches.
	   ((not (or (eq search-invisible t)
		     ;; Don't open overlays for automatic replacements.
		     (and (not query-flag) search-invisible)
		     ;; Open hidden overlays for interactive replacements.
		     (not (isearch-range-invisible
			   (nth 0 real-match-data) (nth 1 real-match-data)))))
	    (setq skip-invisible-count (1+ skip-invisible-count)))
	   (t
	    ;; Calculate the replacement string, if necessary.
	    (when replacements
	      (set-match-data real-match-data)
	      (setq next-replacement
		    (funcall (car replacements) (cdr replacements)
			     replace-count)))
	    (if (not query-flag)
		(progn
		  (unless (or literal noedit)
		    (replace-highlight
		     (nth 0 real-match-data) (nth 1 real-match-data)
		     start end search-string
		     regexp-flag delimited-flag case-fold-search backward))
		  (setq noedit
			(replace-match-maybe-edit
			 next-replacement nocasify literal
			 noedit real-match-data backward)
			replace-count (1+ replace-count)))
	      (undo-boundary)
	      (let (done replaced key def)
		;; Loop reading commands until one of them sets done,
		;; which means it has finished handling this
		;; occurrence.  Any command that sets `done' should
		;; leave behind proper match data for the stack.
		;; Commands not setting `done' need to adjust
		;; `real-match-data'.
		(while (not done)
		  ;; This sets match-data only for the next hook and
		  ;; replace-highlight that calls `sit-for' from
		  ;; isearch-lazy-highlight-new-loop whose redisplay
		  ;; might clobber match-data. So subsequent code should
		  ;; use only real-match-data, not match-data (bug#36328).
		  (set-match-data real-match-data)
                  (run-hooks 'replace-update-post-hook) ; Before `replace-highlight'.
                  (replace-highlight
		   (match-beginning 0) (match-end 0)
		   start end search-string
		   regexp-flag delimited-flag case-fold-search backward)
                  ;; Obtain the matched groups: needed only when
                  ;; regexp-flag non nil.
                  (when (and last-was-undo regexp-flag)
                    (setq last-was-undo nil
                          real-match-data
                          (save-excursion
                            (goto-char (nth 0 real-match-data))
                            (looking-at search-string)
                            (match-data t real-match-data))))
                  ;; Matched string and next-replacement-replaced
                  ;; stored in stack.
                  (setq search-string-replaced (buffer-substring-no-properties
                                                (nth 0 real-match-data)
                                                (nth 1 real-match-data))
                        next-replacement-replaced
                        (query-replace-descr
                         (save-match-data
                           (set-match-data real-match-data)
                           (match-substitute-replacement
                            next-replacement nocasify literal))))
		  (let* ((replacement-presentation
			  (if query-replace-show-replacement
			      (save-match-data
			        (set-match-data real-match-data)
			        (match-substitute-replacement next-replacement
							      nocasify literal))
			    next-replacement))
			 (prompt
			  (format message
                                  (query-replace-descr from-string)
                                  (query-replace-descr
                                   replacement-presentation))))
                    ;; Use `read-key' so that escape sequences on TTYs
                    ;; are properly mapped back to the intended key.
		    (setq key (read-key prompt)))
		  ;; Necessary in case something happens during
		  ;; read-event that clobbers the match data.
		  (set-match-data real-match-data)
		  (setq key (vector key))
		  (setq def (lookup-key map key t))
		  ;; Restore the match data while we process the command.
		  (cond ((eq def 'help)
			 (let ((display-buffer-overriding-action
				'(nil (inhibit-same-window . t))))
			   (with-output-to-temp-buffer "*Help*"
			     (princ
			      (concat "Query replacing "
				      (if backward "backward " "")
				      (if delimited-flag
					  (or (and (symbolp delimited-flag)
						   (get delimited-flag
							'isearch-message-prefix))
					      "word ") "")
				      (if regexp-flag "regexp " "")
				      from-string " with "
				      next-replacement ".\n\n"
				      (substitute-command-keys
				       query-replace-help)))
			     (with-current-buffer standard-output
			       (help-mode)))))
			((eq def 'exit)
			 (setq keep-going nil)
			 (setq done t))
			((eq def 'exit-current)
			 (setq multi-buffer t keep-going nil done t))
			((eq def 'backup)
			 (if stack
			     (let ((elt (pop stack)))
			       (goto-char (nth 0 elt))
			       (setq replaced (nth 1 elt)
				     real-match-data
				     (replace-match-data
				      t real-match-data
				      (nth 2 elt))))
			   (message "No previous match")
			   (ding 'no-terminate)
			   (sit-for 1)))
			((or (eq def 'undo) (eq def 'undo-all))
			 (if (null stack)
                             (progn
                               (message "Nothing to undo")
                               (ding 'no-terminate)
                               (sit-for 1))
			   (let ((stack-idx         0)
                                 (stack-len         (length stack))
                                 (num-replacements  0)
                                 (nocasify t) ; Undo must preserve case (Bug#31073).
                                 search-string
                                 last-replacement)
                             (while (and (< stack-idx stack-len)
                                         stack
                                         (or (null replaced) last-was-act-and-show))
                               (let* ((elt (nth stack-idx stack)))
                                 (setq
                                  stack-idx (1+ stack-idx)
                                  replaced (nth 1 elt)
                                  ;; Bind swapped values
                                  ;; (search-string <--> replacement)
                                  search-string (nth (if replaced 4 3) elt)
                                  last-replacement (nth (if replaced 3 4) elt)
                                  search-string-replaced search-string
                                  next-replacement-replaced last-replacement
                                  last-was-act-and-show nil)

                                 (when (and (= stack-idx stack-len)
                                            (and (null replaced) (not last-was-act-and-show))
                                            (zerop num-replacements))
                                          (message "Nothing to undo")
                                          (ding 'no-terminate)
                                          (sit-for 1))

                                 (when replaced
                                   (setq stack (nthcdr stack-idx stack))
                                   (goto-char (nth 0 elt))
                                   (set-match-data (nth 2 elt))
                                   (setq real-match-data
                                         (save-excursion
                                           (goto-char (match-beginning 0))
                                           ;; We must quote the string (Bug#37073)
                                           (looking-at (regexp-quote search-string))
                                           (match-data t (nth 2 elt)))
                                         noedit
                                         (replace-match-maybe-edit
                                          last-replacement nocasify literal
                                          noedit real-match-data backward)
                                         replace-count (1- replace-count)
                                         real-match-data
                                         (save-excursion
                                           (goto-char (match-beginning 0))
                                           (if regexp-flag
                                               (looking-at last-replacement)
                                             (looking-at (regexp-quote last-replacement)))
                                           (match-data t (nth 2 elt))))
                                   (when regexp-flag
                                     (setq next-replacement (nth 4 elt)))
                                   ;; Set replaced nil to keep in loop
                                   (when (eq def 'undo-all)
                                     (setq replaced nil
                                           stack-len (- stack-len stack-idx)
                                           stack-idx 0
                                           num-replacements
                                           (1+ num-replacements))))))
                             (when (and (eq def 'undo-all)
                                        (null (zerop num-replacements)))
                               (message (ngettext "Undid %d replacement"
                                                  "Undid %d replacements"
                                                  num-replacements)
                                        num-replacements)
                               (ding 'no-terminate)
                               (sit-for 1)))
			   (setq replaced nil last-was-undo t last-was-act-and-show nil)))
			((eq def 'act)
			 (or replaced
			     (setq noedit
				   (replace-match-maybe-edit
				    next-replacement nocasify literal
				    noedit real-match-data backward)
				   replace-count (1+ replace-count)))
			 (setq done t replaced t update-stack (not last-was-act-and-show)))
			((eq def 'act-and-exit)
			 (or replaced
			     (setq noedit
				   (replace-match-maybe-edit
				    next-replacement nocasify literal
				    noedit real-match-data backward)
				   replace-count (1+ replace-count)))
			 (setq keep-going nil)
			 (setq done t replaced t))
			((eq def 'act-and-show)
			 (unless replaced
			     (setq noedit
				   (replace-match-maybe-edit
				    next-replacement nocasify literal
				    noedit real-match-data backward)
				   replace-count (1+ replace-count)
				   real-match-data (replace-match-data
						    t real-match-data)
				   replaced t last-was-act-and-show t)
                             (replace--push-stack
                              replaced
                              search-string-replaced
                              next-replacement-replaced stack)))
			((or (eq def 'automatic) (eq def 'automatic-all))
			 (or replaced
			     (setq noedit
				   (replace-match-maybe-edit
				    next-replacement nocasify literal
				    noedit real-match-data backward)
				   replace-count (1+ replace-count)))
			 (setq done t query-flag nil replaced t)
			 (if (eq def 'automatic-all) (setq multi-buffer t)))
			((eq def 'skip)
			 (setq done t update-stack (not last-was-act-and-show)))
			((eq def 'recenter)
			 ;; `this-command' has the value `query-replace',
			 ;; so we need to bind it to `recenter-top-bottom'
			 ;; to allow it to detect a sequence of `C-l'.
			 (let ((this-command 'recenter-top-bottom)
			       (last-command 'recenter-top-bottom))
			   (recenter-top-bottom)))
			((eq def 'edit)
			 (let ((opos (point-marker))
			       ;; Restore original isearch filter to allow
			       ;; using isearch in a recursive edit even
			       ;; when perform-replace was started from
			       ;; `xref--query-replace-1' that let-binds
			       ;; `isearch-filter-predicate' (bug#53758).
			       (isearch-filter-predicate #'isearch-filter-visible))
			   (setq real-match-data (replace-match-data
						  nil real-match-data
						  real-match-data))
			   (goto-char (match-beginning 0))
			   (save-excursion
			     (save-window-excursion
			       (recursive-edit)))
			   (goto-char opos)
			   (set-marker opos nil))
			 ;; Before we make the replacement,
			 ;; decide whether the search string
			 ;; can match again just after this match.
			 (if (and regexp-flag nonempty-match)
			     (setq match-again (and (looking-at search-string)
						    (match-data)))))
			;; Edit replacement.
			((or (eq def 'edit-replacement)
                             (eq def 'edit-replacement-exact-case))
			 (setq real-match-data (replace-match-data
						nil real-match-data
						real-match-data)
			       next-replacement
			       (read-string
                                (format "Edit replacement string%s: "
                                        (if (eq def
                                                'edit-replacement-exact-case)
                                            " (exact case)"
                                          ""))
                                next-replacement)
			       noedit nil)
			 (if replaced
			     (set-match-data real-match-data)
			   (setq noedit
				 (replace-match-maybe-edit
				  next-replacement
                                  (if (eq def 'edit-replacement-exact-case)
                                      t
                                    nocasify)
                                  literal noedit
				  real-match-data backward)
				 replaced t)
			   (setq next-replacement-replaced next-replacement))
			 (setq done t))

			((eq def 'delete-and-edit)
			 (replace-match "" t t)
			 (setq real-match-data (replace-match-data
						nil real-match-data))
			 (replace-dehighlight)
			 (save-excursion (recursive-edit))
			 (setq replaced t))
                        ((commandp def t)
                         (call-interactively def))
			;; Note: we do not need to treat `exit-prefix'
			;; specially here, since we reread
			;; any unrecognized character.
			(t
			 (setq this-command 'mode-exited)
			 (setq keep-going nil)
			 (setq unread-command-events
			       (append (listify-key-sequence key)
				       unread-command-events))
			 (setq done t)))
		  (when query-replace-lazy-highlight
		    ;; Force lazy rehighlighting only after replacements.
		    (if (not (memq def '(skip backup)))
			(setq isearch-lazy-highlight-last-string nil)))
		  (unless (eq def 'recenter)
		    ;; Reset recenter cycling order to initial position.
		    (setq recenter-last-op nil)))
		;; Record previous position for ^ when we move on.
		;; Change markers to numbers in the match data
		;; since lots of markers slow down editing.
                (when update-stack
                  (replace--push-stack
                   replaced
                   search-string-replaced
                   next-replacement-replaced stack))
                (setq next-replacement-replaced nil
                      search-string-replaced    nil
                      last-was-act-and-show     nil))))))
      (replace-dehighlight)
      (when region-filter
        (remove-function isearch-filter-predicate region-filter))
      (when (and query-flag (fboundp 'set-text-conversion-style))
        ;; Resume text conversion.
        (setq overriding-text-conversion-style
              old-text-conversion-style)
        (set-text-conversion-style text-conversion-style)))
    (or unread-command-events
	(message (ngettext "Replaced %d occurrence%s"
			   "Replaced %d occurrences%s"
			   replace-count)
		 replace-count
		 (if (> (+ skip-read-only-count
			   skip-filtered-count
			   skip-invisible-count)
                        0)
		     (format " (skipped %s)"
			     (mapconcat
			      #'identity
			      (delq nil (list
					 (if (> skip-read-only-count 0)
					     (format "%s read-only"
						     skip-read-only-count))
					 (if (> skip-invisible-count 0)
					     (format "%s invisible"
						     skip-invisible-count))
					 (if (> skip-filtered-count 0)
					     (format "%s filtered out"
						     skip-filtered-count))))
			      ", "))
		   "")))
    (or (and keep-going stack) multi-buffer)))

(provide 'replace)

;;; replace.el ends here
