;;; diff.el --- run `diff'  -*- lexical-binding: t -*-

;; Copyright (C) 1992, 1994, 1996, 2001-2025 Free Software Foundation,
;; Inc.

;; Author: Frank Bresz
;; (according to authors.el)
;; Maintainer: emacs-devel@gnu.org
;; Keywords: unix, vc, tools

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

;; This package helps you explore differences between files, using the
;; UNIX command diff(1).  The commands are `diff' and `diff-backup'.
;; You can specify options with `diff-switches'.

;;; Code:

(declare-function diff-setup-whitespace "diff-mode" ())

(defgroup diff nil
  "Comparing files with `diff'."
  :group 'tools)

;;;###autoload
(defcustom diff-switches "-u"
  "A string or list of strings specifying switches to be passed to diff.

This variable is also used in the `vc-diff' command (and related
commands) if the backend-specific diff switch variable isn't
set (`vc-git-diff-switches' for git, for instance), and
`vc-diff-switches' isn't set."
  :type '(choice string (repeat string)))

;;;###autoload
(defcustom diff-command "diff"
  "The command to use to run diff."
  :type 'string)

(defcustom diff-entire-buffers t
  "If non-nil, diff the entire buffers, not just the visible part.
If nil, only use the narrowed-to parts of the buffers."
  :type 'boolean
  :version "29.1")

;; prompt if prefix arg present
(defun diff-switches ()
  (if current-prefix-arg
      (read-string "Diff switches: "
		   (if (stringp diff-switches)
		       diff-switches
		     (mapconcat #'identity diff-switches " ")))))

(defun diff-sentinel (code &optional old-temp-file new-temp-file)
  "Code run when the diff process exits.
CODE is the exit code of the process.  It should be 0 only if no diffs
were found.
If optional args OLD-TEMP-FILE and/or NEW-TEMP-FILE are non-nil,
delete the temporary files so named."
  (if old-temp-file (delete-file old-temp-file))
  (if new-temp-file (delete-file new-temp-file))
  (diff-setup-whitespace)
  (goto-char (point-min))
  (save-excursion
    (goto-char (point-max))
    (let ((inhibit-read-only t))
      (insert (format "\nDiff finished%s.  %s\n"
		      (cond ((equal 0 code) " (no differences)")
			    ((equal 2 code) " (diff error)")
			    (t ""))
		      (current-time-string))))))

;;;###autoload
(defun diff (old new &optional switches no-async)
  "Find and display the differences between OLD and NEW files.
When called interactively, read NEW, then OLD, using the
minibuffer.  The default for NEW is the current buffer's file
name, and the default for OLD is a backup file for NEW, if one
exists.  If NO-ASYNC is non-nil, call diff synchronously.

When called interactively with a prefix argument SWITCHES, prompt
interactively for diff switches.  Otherwise, the switches
specified in the variable `diff-switches' are passed to the diff
command.

Non-interactively, OLD and NEW may each be a file or a buffer."
  (interactive
   (let* ((newf (if (and buffer-file-name (file-exists-p buffer-file-name))
		    (read-file-name
                     (format-prompt "Diff new file"
                                    (file-name-nondirectory buffer-file-name))
		     nil buffer-file-name t)
		  (read-file-name "Diff new file: " nil nil t)))
          (oldf (file-newest-backup newf)))
     (setq oldf (if (and oldf (file-exists-p oldf))
		    (read-file-name
                     (format-prompt "Diff original file"
                                    (file-name-nondirectory oldf))
		     (file-name-directory oldf) oldf t)
		  (read-file-name "Diff original file: "
				  (file-name-directory newf) nil t)))
     (list oldf newf (diff-switches))))
  (display-buffer
   (diff-no-select old new switches no-async)))

(defun diff-file-local-copy (file-or-buf)
  "Like `file-local-copy' but also supports a buffer as the argument.
When FILE-OR-BUF is a buffer, return the filename of a local
temporary file with the buffer's contents."
  (if (bufferp file-or-buf)
      (with-current-buffer file-or-buf
        (let ((tempfile (make-temp-file "buffer-content-")))
          (if diff-entire-buffers
              (write-region nil nil tempfile nil 'nomessage)
            (write-region (point-min) (point-max) tempfile nil 'nomessage))
          tempfile))
    (file-local-copy file-or-buf)))

(defvar diff-use-labels 'check
  "Whether `diff-command' understands the \"--label\" option.
Possible values are:
  t     -- yes, it does
  nil   -- no, it does not
  check -- try to probe whether it does")

(defvar diff-default-directory)

(defun diff-check-labels (&optional force)
  (if (not (or force (eq 'check diff-use-labels)))
      diff-use-labels
    (setq diff-use-labels
	  (with-temp-buffer
	    (when (ignore-errors
		    (call-process diff-command nil t nil "--help"))
	      (if (search-backward "--label" nil t) t))))))

(defun diff-no-select (old new &optional switches no-async buf)
  ;; Noninteractive helper for creating and reverting diff buffers
  "Compare the OLD and NEW file/buffer.
If the optional SWITCHES is nil, the switches specified in the
variable `diff-switches' are passed to the diff command,
otherwise SWITCHES is used.  SWITCHES can be a string or a list
of strings.

If NO-ASYNC is non-nil, call diff synchronously.

By default, this function creates the diff in the \"*Diff*\"
buffer.  If BUF is non-nil, BUF is used instead.  This function
returns the buffer used."
  (unless (bufferp new) (setq new (expand-file-name new)))
  (unless (bufferp old) (setq old (expand-file-name old)))
  (or switches (setq switches diff-switches)) ; If not specified, use default.
  (setq switches (ensure-list switches))
  (or buf (setq buf (get-buffer-create "*Diff*")))
  (diff-check-labels)
  (let* ((old-alt (diff-file-local-copy old))
	 (new-alt (diff-file-local-copy new))
	 (command
	  (mapconcat #'identity
		     `(,diff-command
		       ;; Use explicitly specified switches
		       ,@switches
                       ,@(mapcar #'shell-quote-argument
                                 (nconc
                                  (and (or old-alt new-alt)
				       (eq diff-use-labels t)
				       (list "--label"
					     (if (stringp old) old
					       (prin1-to-string old))
					     "--label"
					     (if (stringp new) new
					       (prin1-to-string new))))
                                  (list (or old-alt old)
                                        (or new-alt new)))))
		     " "))
	 (thisdir default-directory))
    (with-current-buffer buf
      (setq buffer-read-only t)
      (buffer-disable-undo (current-buffer))
      (let ((inhibit-read-only t))
	(erase-buffer))
      (buffer-enable-undo (current-buffer))
      (diff-mode)
      (setq-local revert-buffer-function
                  (lambda (_ignore-auto _noconfirm)
                    (diff-no-select old new switches no-async (current-buffer))))
      (setq default-directory thisdir)
      (setq diff-default-directory default-directory)
      (let ((inhibit-read-only t))
	(insert command "\n"))
      (with-file-modes #o600
        (if (and (not no-async) (fboundp 'make-process))
	    (let* ((default-directory temporary-file-directory)
                   (proc (start-process "Diff" buf shell-file-name
                                        shell-command-switch command)))
	      (set-process-filter proc #'diff-process-filter)
              (set-process-sentinel
               proc (lambda (proc _msg)
                      (with-current-buffer (process-buffer proc)
                        (diff-sentinel (process-exit-status proc)
                                       old-alt new-alt)))))
	  ;; Async processes aren't available.
	  (let* ((default-directory temporary-file-directory)
                 (inhibit-read-only t))
	    (diff-sentinel
	     (call-process shell-file-name nil buf nil
			   shell-command-switch command)
             old-alt new-alt)))))
    buf))

(defun diff-process-filter (proc string)
  (with-current-buffer (process-buffer proc)
    (let ((moving (= (point) (process-mark proc))))
      (save-excursion
	;; Insert the text, advancing the process marker.
	(goto-char (process-mark proc))
	(let ((inhibit-read-only t))
	  (insert string))
	(set-marker (process-mark proc) (point)))
      (if moving (goto-char (process-mark proc))))))

;;;###autoload
(defun diff-backup (file &optional switches)
  "Diff this file with its backup file or vice versa.
Uses the latest backup, if there are several numerical backups.
If this file is a backup, diff it with its original.
The backup file is the first file given to `diff'.
With prefix arg SWITCHES, prompt for diff switches."
  (interactive (list (read-file-name "Diff (file with backup): ")
		     (diff-switches)))
  (let (bak ori)
    (if (backup-file-name-p file)
	(setq bak file
	      ori (file-name-sans-versions file))
      (setq bak (or (diff-latest-backup-file file)
		    (error "No backup found for %s" file))
	    ori file))
    (diff bak ori switches)))

;;;###autoload
(defun diff-latest-backup-file (fn)
  "Return the latest existing backup of file FN, or nil."
  (let ((handler (find-file-name-handler fn 'diff-latest-backup-file)))
    (if handler
	(funcall handler 'diff-latest-backup-file fn)
      (file-newest-backup fn))))

;;;###autoload
(defun diff-buffer-with-file (&optional buffer)
  "View the differences between BUFFER and its associated file.
This requires the external program `diff' to be in your `exec-path'."
  (interactive "bBuffer: ")
  (let ((buf (get-buffer (or buffer (current-buffer)))))
    (with-current-buffer (or (buffer-base-buffer buf) buf)
      (unless buffer-file-name
        (error "Buffer is not visiting a file"))
      (diff buffer-file-name (current-buffer) nil 'noasync))))

;;;###autoload
(defun diff-buffers (old new &optional switches no-async)
  "Find and display the differences between OLD and NEW buffers.

When called interactively, read NEW, then OLD, using the
minibuffer.  The default for NEW is the current buffer, and the
default for OLD is the most recently selected other buffer.
If NO-ASYNC is non-nil, call diff synchronously.

When called interactively with a prefix argument, prompt
interactively for diff switches.  Otherwise, the switches
specified in the variable `diff-switches' are passed to the
diff command.

OLD and NEW may each be a buffer or a buffer name.

Also see the `diff-entire-buffers' variable."
  (interactive
   (let ((newb (read-buffer "Diff new buffer" (current-buffer) t))
         (oldb (read-buffer "Diff original buffer"
                            (other-buffer (current-buffer) t) t)))
     (list oldb newb (diff-switches))))
  (diff (get-buffer old) (get-buffer new) switches no-async))

(provide 'diff)

;;; diff.el ends here
