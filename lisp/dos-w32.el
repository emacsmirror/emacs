;;; dos-w32.el --- Functions shared among MS-DOS and W32 (NT/95) platforms  -*- lexical-binding: t; -*-

;; Copyright (C) 1996, 2001-2025 Free Software Foundation, Inc.

;; Maintainer: Geoff Voelker <voelker@cs.washington.edu>
;; Keywords: internal
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

;; Parts of this code are duplicated functions taken from dos-fns.el
;; and winnt.el.

;;; Code:

;; Use ";" instead of ":" as a path separator (from files.el).
(when (memq system-type '(ms-dos windows-nt))
  (setq path-separator ";")
  (push 'file-name-history minibuffer-history-case-insensitive-variables)
  ;; Set the null device (for compile.el).
  (setq null-device "NUL")
  (setq-default buffer-file-coding-system 'undecided-dos))

;; For distinguishing file types based upon suffixes.  DEPRECATED, DO NOT USE!
(defcustom file-name-buffer-file-type-alist
  '(("[:/].*config.sys$" . nil)		; config.sys text
    ("\\.\\(obj\\|exe\\|com\\|lib\\|sys\\|bin\\|ico\\|pif\\|class\\)$" . t)
					; MS-Dos stuff
    ("\\.\\(dll\\|drv\\|386\\|vxd\\|fon\\|fnt\\|fot\\|ttf\\|grp\\)$" . t)
					; Windows stuff
    ("\\.\\(bmp\\|wav\\|avi\\|mpg\\|jpg\\|tif\\|mov\\|au\\)$" . t)
					; known binary data files
    ("\\.\\(arc\\|zip\\|pak\\|lzh\\|zoo\\)$" . t)
					; Packers
    ("\\.\\(a\\|o\\|tar\\|z\\|gz\\|taz\\|jar\\)$" . t)
					; Unix stuff
    ("\\.sx[dmicw]$" . t)		; OpenOffice.org
    ("\\.tp[ulpw]$" . t)		; borland Pascal stuff
    ("[:/]tags$" . nil)			; emacs TAGS file
    )
  "Alist used in the past for distinguishing text files from binary files.
Each element has the form (REGEXP . TYPE), where REGEXP is matched
against the file name, and TYPE is nil for text, t for binary.

This variable is deprecated, not used anywhere, and will soon be deleted."
  :type '(repeat (cons regexp boolean))
  :group 'dos-fns
  :group 'w32)

(make-obsolete-variable 'file-name-buffer-file-type-alist
			'file-coding-system-alist
			"24.4")

(defun find-buffer-file-type-coding-system (command)
  "Choose a coding system for a file operation in COMMAND.
COMMAND is a list that specifies the operation, an I/O primitive, as its
CAR, and the arguments that might be given to that operation as its CDR.
If operation is `insert-file-contents', the coding system is chosen based
upon the filename (the CAR of the arguments beyond the operation), the contents
of `w32-untranslated-filesystem-list' and `file-name-buffer-file-type-alist',
and whether the file exists:

  If it matches in `w32-untranslated-filesystem-list':
    If the file exists:					`undecided'
    If the file does not exist:				`undecided-unix'
  Otherwise:
    If the file exists:					`undecided'
    If the file does not exist   default value of `buffer-file-coding-system'

Note that the CAR of arguments to `insert-file-contents' operation could
be a cons cell of the form (FILENAME . BUFFER), where BUFFER is a buffer
into which the file's contents were already read, but not yet decoded.

If operation is `write-region', the coding system is chosen based
upon the value of `buffer-file-coding-system'.  If
`buffer-file-coding-system' is non-nil, its value is used.
Otherwise, it is `undecided-dos'.

The most common situation is when DOS and Unix files are read and
written, and their names do not match in `w32-untranslated-filesystem-list'.
In these cases, the coding system initially will be `undecided'.
As the file is read in the DOS case, the coding system will be
changed to `undecided-dos' as CR/LFs are detected.  As the file
is read in the Unix case, the coding system will be changed to
`undecided-unix' as LFs are detected.  In both cases,
`buffer-file-coding-system' will be set to the appropriate coding
system, and the value of `buffer-file-coding-system' will be used
when writing the file."

  (let ((op (nth 0 command))
	(undecided nil) (undecided-unix nil)
	target target-buf)
    (cond ((eq op 'insert-file-contents)
	   (setq target (nth 1 command))
	   ;; If TARGET is a cons cell, it has the form (FILENAME . BUFFER),
	   ;; where BUFFER is a buffer into which the file was already read,
	   ;; but its contents were not yet decoded.  (This form of the
	   ;; arguments is used, e.g., in arc-mode.el.)  This function
	   ;; doesn't care about the contents, it only looks at the file's
	   ;; name, which is the CAR of the cons cell.
	   (when (consp target)
	     (setq target-buf
		   (and (bufferp (cdr target))
			(buffer-name (cdr target))))
	     (setq target (car target)))
	   (cond ((or
		   ;; For any existing file, decide based on contents.
		   (file-exists-p target)
		   ;; If TARGET does not exist as a file, replace its
		   ;; base name with TARGET-BUF and try again.  This
		   ;; is for jka-compr's sake, which strips the
		   ;; compression (.gz etc.) extension from the
		   ;; FILENAME, but leaves it in the BUFFER's name.
		   (and (stringp target-buf)
			(file-exists-p
			 (expand-file-name target-buf
					   (file-name-directory target)))))
		  (setq undecided t))
		 ;; Next check for a non-DOS file system.
		 ((w32-untranslated-file-p target)
		  (setq undecided-unix t)))
	   (cond (undecided-unix '(undecided-unix . undecided-unix))
		 (undecided '(undecided . undecided))
		 (t (cons (default-value 'buffer-file-coding-system)
			  (default-value 'buffer-file-coding-system)))))
	  ((eq op 'write-region)
	   (if buffer-file-coding-system
	       (cons buffer-file-coding-system
		     buffer-file-coding-system)
	     ;; Normally this is used only in a non-file-visiting
	     ;; buffer, because normally buffer-file-coding-system is non-nil
	     ;; in a file-visiting buffer.
	     '(undecided-dos . undecided-dos))))))
(make-obsolete 'find-buffer-file-type-coding-system nil "24.4")

(defun find-file-binary (filename)
  "Visit file FILENAME and treat it as binary."
  ;; FIXME: Why here rather than in files.el?
  ;; FIXME: Can't we use find-file-literally for the same purposes?
  (interactive "FFind file binary: ")
  (let ((coding-system-for-read 'no-conversion))  ;; FIXME: undecided-unix?
    (with-suppressed-warnings ((interactive-only find-file))
      (find-file filename))))

(defun find-file-text (filename)
  "Visit file FILENAME and treat it as a text file."
  (interactive "FFind file text: ")
  (let ((coding-system-for-read 'undecided-dos))
    (with-suppressed-warnings ((interactive-only find-file))
      (find-file filename))))

(defun w32-find-file-not-found-set-buffer-file-coding-system ()
  (with-current-buffer (current-buffer)
    (let ((coding buffer-file-coding-system))
      ;; buffer-file-coding-system is already set by
      ;; find-operation-coding-system, which was called from
      ;; insert-file-contents.  All that's left is to change
      ;; the EOL conversion, if required by the user.
      (when (and (null coding-system-for-read)
		 (or inhibit-eol-conversion
		     (w32-untranslated-file-p (buffer-file-name))))
	(setq coding (coding-system-change-eol-conversion coding 0))
	(setq buffer-file-coding-system coding))
      nil)))

;; To set the default coding system on new files.
(add-hook 'find-file-not-found-functions
	  'w32-find-file-not-found-set-buffer-file-coding-system)

;;; To accommodate filesystems that do not require CR/LF translation.
(define-obsolete-variable-alias 'untranslated-filesystem-list
  'w32-untranslated-filesystem-list "24.4")
(defvar w32-untranslated-filesystem-list nil
  "List of filesystems that require no CR/LF translation when reading
and writing files.  Each filesystem in the list is a string naming
the directory prefix corresponding to the filesystem.")

(defun w32-untranslated-canonical-name (filename)
  "Return FILENAME in a canonicalized form for use with the functions
dealing with untranslated filesystems."
  (if (memq system-type '(ms-dos windows-nt cygwin))
      ;; The canonical form for DOS/W32 is with A-Z downcased and all
      ;; directory separators changed to directory-sep-char.
      (let ((name
             (mapconcat (lambda (char)
                          (char-to-string (if (and (<= ?A char ?Z))
                                              (+ (- char ?A) ?a)
                                            char)))
                        filename nil)))
	;; Use expand-file-name to canonicalize directory separators, except
	;; with bare drive letters (which would have the cwd appended).
	;; Avoid expanding names that could trigger ange-ftp to prompt
	;; for passwords, though.
	(if (or (string-match-p "^.:\\'" name)
		(string-match-p "^/[^/:]+:" name))
	    name
	  (expand-file-name name)))
    filename))

(defun w32-untranslated-file-p (filename)
  "Return t if FILENAME is on a filesystem that does not require
CR/LF translation, and nil otherwise."
  (let ((fs (w32-untranslated-canonical-name filename))
	(ufs-list w32-untranslated-filesystem-list)
	(found nil))
    (while (and (not found) ufs-list)
      (if (string-match-p (concat "^" (car ufs-list)) fs)
	  (setq found t)
	(setq ufs-list (cdr ufs-list))))
    found))

(define-obsolete-function-alias 'add-untranslated-filesystem
  'w32-add-untranslated-filesystem "24.4")
(defun w32-add-untranslated-filesystem (filesystem)
  "Add FILESYSTEM to the list of filesystems that do not require
CR/LF translation.  FILESYSTEM is a string containing the directory
prefix corresponding to the filesystem.  For example, for a Unix
filesystem mounted on drive Z:, FILESYSTEM could be \"Z:\"."
  ;; We use "D", not "f", to avoid confusing the user: "f" prompts
  ;; with a directory, but RET returns the current buffer's file, not
  ;; its directory.
  (interactive "DUntranslated file system: ")
  (let ((fs (w32-untranslated-canonical-name filesystem)))
    (if (member fs w32-untranslated-filesystem-list)
	w32-untranslated-filesystem-list
      (push fs w32-untranslated-filesystem-list))))


(define-obsolete-function-alias 'remove-untranslated-filesystem
  'w32-remove-untranslated-filesystem "24.4")
(defun w32-remove-untranslated-filesystem (filesystem)
  "Remove FILESYSTEM from the list of filesystems that do not require
CR/LF translation.  FILESYSTEM is a string containing the directory
prefix corresponding to the filesystem.  For example, for a Unix
filesystem mounted on drive Z:, FILESYSTEM could be \"Z:\"."
  (interactive "fUntranslated file system: ")
  (setq w32-untranslated-filesystem-list
	(delete (w32-untranslated-canonical-name filesystem)
		w32-untranslated-filesystem-list)))

;;; Support for printing under DOS/Windows, see lpr.el and ps-print.el.

(define-obsolete-variable-alias 'direct-print-region-use-command-dot-com
  'w32-direct-print-region-use-command-dot-com "24.4")
(defcustom w32-direct-print-region-use-command-dot-com t
  "If non-nil, use command.com to print on Windows 9x."
  :type 'boolean
  :group 'dos-fns
  :group 'w32)

(defvar w32-quote-process-args)

;; Function to actually send data to the printer port.
;; Supports writing directly, and using various programs.
(defun w32-direct-print-region-helper (printer
                                   start end
                                   lpr-prog
                                   _delete-text _buf _display
                                   rest)
  (let* (;; Ignore case when matching known external program names.
	 (case-fold-search t)
	 ;; Convert / to \ in printer name, for sake of external programs.
	 (printer
	  (if (stringp printer)
	      (subst-char-in-string ?/ ?\\ printer)
	    printer))
	 ;; Find a directory that is local, to work-around Windows bug.
	 (safe-dir
	  (let ((safe-dirs (list "c:/" (getenv "windir") (getenv "TMPDIR"))))
	    (while (not (file-attributes (car safe-dirs)))
	      (setq safe-dirs (cdr safe-dirs)))
	    (car safe-dirs)))
	 (tempfile
	  (subst-char-in-string
	   ?/ ?\\
	   (make-temp-name
	    (expand-file-name "EP" temporary-file-directory))))
	 ;; capture output for diagnosis
	 (errbuf (list (get-buffer-create " *print-region-helper*") t)))
    ;; It seems that we must be careful about the directory name that
    ;; gets added to the printer port name by write-region when using
    ;; the standard "PRN" or "LPTx" ports, because the write can fail if
    ;; the directory is on a network drive.  The same is true when
    ;; asking command.com to copy the file.
    ;; No action is needed for UNC printer names, which is just as well
    ;; because `expand-file-name' doesn't support UNC names on MS-DOS.
    (if (and (stringp printer) (not (string-match-p "^\\\\" printer)))
	(setq printer
	      (subst-char-in-string ?/ ?\\ (expand-file-name printer safe-dir))))
    ;; Handle known programs specially where necessary.
    (unwind-protect
	(cond
	 ;; nprint.exe is the standard print command on Netware
	 ((string-match-p "\\`nprint\\(\\.exe\\)?\\'"
                          (file-name-nondirectory lpr-prog))
	  (write-region start end tempfile nil 0)
	  (call-process lpr-prog nil errbuf nil
			tempfile (concat "P=" printer)))
	 ;; print.exe is a standard command on NT
	 ((string-match-p "\\`print\\(\\.exe\\)?\\'"
                          (file-name-nondirectory lpr-prog))
	  ;; Be careful not to invoke print.exe on MS-DOS or Windows 9x
	  ;; though, because it is a TSR program there (hangs Emacs).
	  (or (and (eq system-type 'windows-nt)
		   (null (getenv "winbootdir")))
	      (error "Printing via print.exe is not supported on MS-DOS or Windows 9x"))
	  ;; It seems that print.exe always appends a form-feed so we
	  ;; should make sure to omit the last FF in the data.
	  (if (and (> end start)
		   (char-equal (char-before end) ?\C-l))
	      (setq end (1- end)))
	  ;; cancel out annotate function for non-PS case
	  (let ((write-region-annotate-functions nil))
	    (write-region start end tempfile nil 0))
	  (call-process lpr-prog nil errbuf nil
			(concat "/D:" printer) tempfile))
	 ;; support lpr and similar programs for convenience, but
	 ;; supply an explicit filename because the NT version of lpr
	 ;; can't read from stdin.
	 ((> (length lpr-prog) 0)
	  (write-region start end tempfile nil 0)
	  (setq rest (append rest (list tempfile)))
	  (apply 'call-process lpr-prog nil errbuf nil rest))
	 ;; Run command.com to access printer port on Windows 9x, unless
	 ;; we are supposed to append to an existing (non-empty) file,
	 ;; to work around a bug in Windows 9x that prevents Windows
	 ;; programs from accessing LPT ports reliably.
	 ((and (eq system-type 'windows-nt)
	       (getenv "winbootdir")
	       ;; Allow cop-out so command.com isn't invoked
	       w32-direct-print-region-use-command-dot-com
	       ;; file-attributes fails on LPT ports on Windows 9x but
	       ;; not on NT, so handle both cases for safety.
	       (eq (or (file-attribute-size (file-attributes printer)) 0) 0))
	  (write-region start end tempfile nil 0)
	  (let ((w32-quote-process-args nil))
	    (call-process "command.com" nil errbuf nil "/c"
			  (format "copy /b %s %s" tempfile printer))))
	 ;; write directly to the printer port
	 (t
	  (write-region start end printer t 0)))
      ;; ensure we remove the tempfile if created
      (if (file-exists-p tempfile)
	  (delete-file tempfile)))))

(defvar printer-name)

(declare-function default-printer-name "w32fns.c")

(define-obsolete-function-alias 'direct-print-region-function
  'w32-direct-print-region-function "24.4")
(defun w32-direct-print-region-function (start end
                                               &optional lpr-prog
                                               delete-text buf display
                                               &rest rest)
  "DOS/Windows-specific function to print the region on a printer.
Writes the region to the device or file which is a value of
`printer-name' (which see), unless the value of `lpr-command'
indicates a specific program should be invoked."

  ;; DOS printers need the lines to end with CR-LF pairs, so make
  ;; sure it always happens that way, unless the buffer is binary.
  (let* ((coding coding-system-for-write)
	 (coding-base
	  (if (null coding) 'undecided (coding-system-base coding)))
	 (eol-type (coding-system-eol-type coding-base))
	 ;; Make each print-out eject the final page, but don't waste
	 ;; paper if the file ends with a form-feed already.
	 (write-region-annotate-functions
	  (cons
	   (lambda (_start end)
	     (if (not (char-equal (char-before end) ?\f))
		 `((,end . "\f"))))
	   write-region-annotate-functions))
	 (printer (or (and (boundp 'dos-printer)
			   (stringp (symbol-value 'dos-printer))
			   (symbol-value 'dos-printer))
		      printer-name
		      (default-printer-name))))
    (or (eq coding-system-for-write 'no-conversion)
	(setq coding-system-for-write
	      (aref eol-type 1)))	; force conversion to DOS EOLs
    (w32-direct-print-region-helper printer start end lpr-prog
                                    delete-text buf display rest)))

(defvar lpr-headers-switches)

;; Set this to nil if you have a port of the `pr' program
;; (e.g., from GNU Textutils), or if you have an `lpr'
;; program (see above) that can print page headers.
;; If `lpr-headers-switches' is non-nil (the default) and
;; `print-region-function' is set to `dos-print-region-function',
;; then requests to print page headers will be silently
;; ignored, and `print-buffer' and `print-region' produce
;; the same output as `lpr-buffer' and `lpr-region', accordingly.
(when (memq system-type '(ms-dos windows-nt))
  (setq lpr-headers-switches "(page headers are not supported)"))

(defvar ps-printer-name)

(define-obsolete-function-alias 'direct-ps-print-region-function
  'w32-direct-ps-print-region-function "24.4")
(defun w32-direct-ps-print-region-function (start end
                                                  &optional lpr-prog
                                                  delete-text buf display
                                                  &rest rest)
  "DOS/Windows-specific function to print the region on a PostScript printer.
Writes the region to the device or file which is a value of
`ps-printer-name' (which see), unless the value of `ps-lpr-command'
indicates a specific program should be invoked."

  (let ((printer (or (and (boundp 'dos-ps-printer)
			  (stringp (symbol-value 'dos-ps-printer))
			  (symbol-value 'dos-ps-printer))
		     ps-printer-name
		     (default-printer-name))))
    (w32-direct-print-region-helper printer start end lpr-prog
                                    delete-text buf display rest)))

;(setq ps-lpr-command "gs")

;(setq ps-lpr-switches '("-q" "-dNOPAUSE" "-sDEVICE=epson" "-r240x60"
;			  "-sOutputFile=LPT1"))

(provide 'dos-w32)

;;; dos-w32.el ends here
