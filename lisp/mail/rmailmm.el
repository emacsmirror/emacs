;;; rmailmm.el --- MIME decoding and display stuff for RMAIL  -*- lexical-binding: t; -*-

;; Copyright (C) 2006-2025 Free Software Foundation, Inc.

;; Author: Alexander Pohoyda
;;	Alex Schroeder
;; Maintainer: emacs-devel@gnu.org
;; Keywords: mail
;; Package: rmail

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

;; Essentially based on the design of Alexander Pohoyda's MIME
;; extensions (mime-display.el and mime.el).

;; This file provides two operation modes for viewing a MIME message.

;; (1) When rmail-enable-mime is non-nil (now it is the default), the
;; function `rmail-show-mime' is automatically called.  That function
;; shows a MIME message directly in RMAIL's view buffer.

;; (2) When rmail-enable-mime is nil, the command 'v' (or M-x
;; rmail-mime) shows a MIME message in a new buffer "*RMAIL*".

;; Both operations share the intermediate functions rmail-mime-process
;; and rmail-mime-process-multipart as below.

;; rmail-show-mime
;;   +- rmail-mime-parse
;;   |    +- rmail-mime-process <--+------------+
;;   |         |         +---------+            |
;;   |         + rmail-mime-process-multipart --+
;;   |
;;   + rmail-mime-insert <----------------+
;;       +- rmail-mime-insert-text        |
;;       +- rmail-mime-insert-bulk        |
;;       +- rmail-mime-insert-multipart --+
;;
;; rmail-mime
;;  +- rmail-mime-show <----------------------------------+
;;       +- rmail-mime-process                            |
;;            +- rmail-mime-handle                        |
;;                 +- rmail-mime-text-handler             |
;;                 +- rmail-mime-bulk-handler             |
;;                 |    + rmail-mime-insert-bulk
;;                 +- rmail-mime-multipart-handler        |
;;                      +- rmail-mime-process-multipart --+

;; In addition, for the case of rmail-enable-mime being non-nil, this
;; file provides two functions rmail-insert-mime-forwarded-message and
;; rmail-insert-mime-resent-message for composing forwarded and resent
;; messages respectively.

;; Todo:

;; Make rmail-mime-media-type-handlers-alist usable in the first
;; operation mode.
;; Handle multipart/alternative in the second operation mode.
;; Offer the option to call external/internal viewers (doc-view, xpdf, etc).

;;; Code:

(require 'rmail)
(require 'mail-parse)
(require 'message)
(require 'cl-lib)

;;; User options.

(defgroup rmail-mime nil
  "Rmail MIME handling options."
  :prefix "rmail-mime-"
  :group 'rmail)

(defcustom rmail-mime-media-type-handlers-alist
  '(("multipart/.*" rmail-mime-multipart-handler)
    ("text/.*" rmail-mime-text-handler)
    ("text/\\(x-\\)?patch" rmail-mime-bulk-handler)
    ("\\(image\\|audio\\|video\\|application\\)/.*" rmail-mime-bulk-handler))
  "Functions to handle various content types.
This is an alist with elements of the form (REGEXP FUNCTION ...).
The first item is a regular expression matching a content-type.
The remaining elements are handler functions to run, in order of
decreasing preference.  These are called until one returns non-nil.
Note that this only applies to items with an inline Content-Disposition,
all others are handled by `rmail-mime-bulk-handler'.
Note also that this alist is ignored when the variable
`rmail-enable-mime' is non-nil."
  :type '(alist :key-type regexp :value-type (repeat function))
  :version "23.1")

(defcustom rmail-mime-attachment-dirs-alist
  `(("text/.*" "~/Documents")
    ("image/.*" "~/Pictures")
    (".*" "~/Desktop" "~" ,temporary-file-directory))
  "Default directories to save attachments of various types into.
This is an alist with elements of the form (REGEXP DIR ...).
The first item is a regular expression matching a content-type.
The remaining elements are directories, in order of decreasing preference.
The first directory that exists is used."
  :type '(alist :key-type regexp :value-type (repeat directory))
  :version "23.1")

(defcustom rmail-mime-show-images 'button
  "What to do with image attachments that Emacs is capable of displaying.
If nil, do nothing special.  If `button', add an extra button
that when pushed displays the image in the buffer.  If a number,
automatically show images if they are smaller than that size (in
bytes), otherwise add a display button.  Anything else means to
automatically display the image in the buffer."
  :type '(choice (const :tag "Add button to view image" button)
		 (const :tag "No special treatment" nil)
		 (number :tag "Show if smaller than certain size")
		 (other :tag "Always show" show))
  :version "23.2")

(defcustom rmail-mime-render-html-function
  (cond ((fboundp 'libxml-parse-html-region) #'rmail-mime-render-html-shr)
	((executable-find "lynx") #'rmail-mime-render-html-lynx)
	(t nil))
  "Function to convert HTML to text.
Called with buffer containing HTML extracted from message in a
temporary buffer.  Converts to text in current buffer.  If nil,
display HTML source."
  :group 'rmail
  :version "25.1"
  :type '(choice function (const nil)))

(defcustom rmail-mime-prefer-html
  ;; Default to preferring HTML parts, but only if we have a renderer
  (if rmail-mime-render-html-function t nil)
  "If non-nil, default to showing HTML part rather than text part
when both are available."
  :group 'rmail
  :version "25.1"
  :type 'boolean)

;;; End of user options.

;;; Global variables that always have let-binding when referred.

(defvar rmail-mime-mbox-buffer nil
  "Buffer containing the mbox data.
The value is usually nil, and bound to a proper value while
processing MIME.")

(defvar rmail-mime-view-buffer nil
  "Buffer showing a message.
The value is usually nil, and bound to a proper value while
processing MIME.")

(defvar rmail-mime-coding-system nil
  "The first coding-system used for decoding a MIME entity.
The value is usually nil, and bound to non-nil while inserting
MIME entities.")

(defvar rmail-mime-searching nil
  "Bound to t inside `rmail-search-mime-message' to suppress expensive
operations such as HTML decoding")

;;; MIME-entity object

(cl-defstruct (rmail-mime-entity
               (:copier nil) (:constructor nil)
               (:constructor rmail-mime-entity
		( type disposition transfer-encoding
		  display header tagline body children handler
		  &optional truncated)
  "Return a newly created MIME-entity object from arguments.

A MIME-entity is a vector of 10 elements:

  [TYPE DISPOSITION TRANSFER-ENCODING DISPLAY HEADER TAGLINE BODY
   CHILDREN HANDLER TRUNCATED]

TYPE and DISPOSITION correspond to MIME headers Content-Type and
Content-Disposition respectively, and have this format:

  (VALUE (ATTRIBUTE . VALUE) (ATTRIBUTE . VALUE) ...)

Each VALUE is a string and each ATTRIBUTE is a string.

Consider the following header, for example:

Content-Type: multipart/mixed;
	boundary=\"----=_NextPart_000_0104_01C617E4.BDEC4C40\"

The corresponding TYPE argument must be:

\(\"multipart/mixed\"
  (\"boundary\" . \"----=_NextPart_000_0104_01C617E4.BDEC4C40\"))

TRANSFER-ENCODING corresponds to MIME header
Content-Transfer-Encoding, and is a lower-case string.

DISPLAY is a vector [CURRENT NEW], where CURRENT indicates how
the header, tag line, and body of the entity are displayed now,
and NEW indicates how their display should be updated.
Both elements are `rmail-mime-display' objects.

HEADER and BODY are vectors [BEG END DISPLAY-FLAG], where BEG and
END are markers that specify the region of the header or body lines
in RMAIL's data (mbox) buffer, and DISPLAY-FLAG non-nil means that the
header or body is, by default, displayed by the decoded
presentation form.

TAGLINE is a vector [TAG BULK-DATA DISPLAY-FLAG], where TAG is a
string indicating the depth and index number of the entity,
BULK-DATA is a cons (SIZE . TYPE) indicating the size and type of
an attached data, DISPLAY-FLAG non-nil means that the tag line is
displayed by default.

CHILDREN is a list of child MIME-entities.  A \"multipart/*\"
entity has one or more children.  A \"message/rfc822\" entity
has just one child.  Any other entity has no child.

HANDLER is a function to insert the entity according to DISPLAY.
It is called with one argument ENTITY.

TRUNCATED is non-nil if the text of this entity was truncated."))
  type disposition transfer-encoding
  display header tagline body children handler truncated)

(defsubst rmail-mime-entity-set-truncated (entity truncated)
  (declare (obsolete (setf rmail-mime-entity-truncated) "28.1"))
  (setf (rmail-mime-entity-truncated entity) truncated))

;;; Buttons

(defcustom rmail-mime-save-action nil
  "Action to perform after saving a MIME attachment in Rmail.
The value can be one of the following:

- nil: Do nothing (default).
- `visit-file': Visit the saved file in Emacs.
- `visit-directory': Visit the file's directory in Dired.
- `open-external': Open the file with an external program.
- A function: Call the function with the absolute filename as argument.

Email attachments can be dangerous.  When this variable is set to one of
the predefined actions, the user will be prompted to confirm the action
before it is performed.  If you set this variable to a function, it will
be called without confirmation.  Please exercise caution."
  :type '(choice (const :tag "Do nothing" nil)
                 (const :tag "Visit file in Emacs" visit-file)
                 (const :tag "Visit directory in Dired" visit-directory)
                 (const :tag "Open with external program" open-external)
                 (function :tag "Custom function"))
  :version "31.1")

(defun rmail-mime-save (button)
  "Save the attachment using info in the BUTTON."
  (let* ((rmail-mime-mbox-buffer rmail-view-buffer)
	 (filename (button-get button 'filename))
	 (directory (button-get button 'directory))
	 (data (button-get button 'data))
	 (ofilename filename))
    (if (and (not (stringp data))
	     (rmail-mime-entity-truncated data))
	(unless (y-or-n-p "This entity is truncated; save anyway? ")
	  (error "Aborted")))
    (setq filename (expand-file-name
                    (read-file-name (format-prompt "Save as" filename)
				    directory
				    (expand-file-name filename directory))
		    directory))
    ;; If arg is just a directory, use the default file name, but in
    ;; that directory (copied from write-file).
    (if (file-directory-p filename)
	(setq filename (expand-file-name
			(file-name-nondirectory ofilename)
			(file-name-as-directory filename))))
    (with-temp-buffer
      (set-buffer-file-coding-system 'no-conversion)
      ;; Needed e.g. by jka-compr, so if the attachment is a compressed
      ;; file, the magic signature compares equal with the unibyte
      ;; signature string recorded in jka-compr-compression-info-list.
      (set-buffer-multibyte nil)
      (setq buffer-undo-list t)
      (if (stringp data)
	  (insert data)
	;; DATA is a MIME-entity object.
	(let ((transfer-encoding (rmail-mime-entity-transfer-encoding data))
	      (body (rmail-mime-entity-body data)))
	  (insert-buffer-substring rmail-mime-mbox-buffer
				   (aref body 0) (aref body 1))
	  (cond ((string= transfer-encoding "base64")
		 (ignore-errors (base64-decode-region (point-min) (point-max))))
		((string= transfer-encoding "quoted-printable")
		 (quoted-printable-decode-region (point-min) (point-max))))))
      (write-region nil nil filename nil nil nil t))
    (pcase rmail-mime-save-action
      ('nil nil)
      ('visit-file
       (when (yes-or-no-p (format "Visit attachment `%s' in Emacs? "
                                  (file-name-nondirectory filename)))
         (find-file filename)))
      ('visit-directory
       (when (yes-or-no-p (format "Visit attachment `%s' in Dired? "
                                  (file-name-nondirectory filename)))
         (dired-jump nil filename)))
      ('open-external
       (when (yes-or-no-p (format "Open attachment `%s' with external program? "
                                  (file-name-nondirectory filename)))
         (shell-command-do-open (list filename))))
      ((pred functionp) (funcall rmail-mime-save-action filename)))))

(define-button-type 'rmail-mime-save 'action 'rmail-mime-save)

;; Display options returned by rmail-mime-entity-display.
;; Value is on of nil, t, raw.
(cl-defstruct (rmail-mime-display
               (:copier rmail-mime--copy-display) (:constructor nil)
               (:constructor rmail-mime--make-display (header tagline body)
                "Make an object describing how to display.
Each field's value is a symbol for the corresponding
item with these values:
  nil: not displayed
  t:   displayed by the decoded presentation form
  raw: displayed by the raw MIME data (for the header and body only)."))
  header tagline body)

(defun rmail-mime-entity-segment (pos &optional entity)
  "Return a vector describing the displayed region of a MIME-entity at POS.
Optional 2nd argument ENTITY is the MIME-entity at POS.
The value is a vector [INDEX HEADER TAGLINE BODY END], where
  INDEX: index into the returned vector indicating where POS is (1..3)
  HEADER: the position of the beginning of a header
  TAGLINE: the position of the beginning of a tag line, including
           the newline that precedes it
  BODY: the position of the beginning of a body
  END: the position of the end of the entity."
  (save-excursion
    (or entity
	(setq entity (get-text-property pos 'rmail-mime-entity)))
    (if (not entity)
	(vector 1 (point) (point) (point) (point))
      (let ((current (aref (rmail-mime-entity-display entity) 0))
	    (beg (if (and (> pos (point-min))
			  (eq (get-text-property (1- pos) 'rmail-mime-entity)
			      entity))
		     (previous-single-property-change pos 'rmail-mime-entity
						      nil (point-min))
		   pos))
	    (index 1)
	    tagline-beg body-beg end)
	(goto-char beg)
	;; If the header is displayed, get past it to the tagline.
	(if (rmail-mime-display-header current)
	    (search-forward "\n\n" nil t))
	(setq tagline-beg (point))
	(if (>= pos tagline-beg)
	    (setq index 2))
	;; If the tagline is displayed, get past it to the body.
	(if (rmail-mime-display-tagline current)
	    ;; The next forward-line call must be in sync with how
	    ;; `rmail-mime-insert-tagline' formats the tagline.  The
	    ;; body begins after the empty line that ends the tagline.
	    (forward-line 3))
	(setq body-beg (point))
	(if (>= pos body-beg)
	    (setq index 3))
	;; If the body is displayed, find its end.
	(if (rmail-mime-display-body current)
	    (let ((tag (aref (rmail-mime-entity-tagline entity) 0))
		  tag2)
	      (setq end (next-single-property-change beg 'rmail-mime-entity
						     nil (point-max)))
	      ;; `tag' is either an empty string or "/n" where n is
	      ;; the number of the part of the multipart MIME message.
	      ;; The loop below finds the next location whose
	      ;; `rmail-mime-entity' property specifies a tag of a
	      ;; different value.
	      (while (and (< end (point-max))
			  (setq entity (get-text-property end 'rmail-mime-entity)
				tag2 (aref (rmail-mime-entity-tagline entity) 0))
			  (and (> (length tag2) 0)
			       (eq (string-match tag tag2) 0)))
		(setq end (next-single-property-change end 'rmail-mime-entity
						       nil (point-max)))))
	  (setq end body-beg))
	(vector index beg tagline-beg body-beg end)))))

(defun rmail-mime-shown-mode (entity)
  "Make MIME-entity ENTITY display in the default way."
  (let ((new (aref (rmail-mime-entity-display entity) 1)))
    (setf (rmail-mime-display-header new)
          (aref (rmail-mime-entity-header entity) 2))
    (setf (rmail-mime-display-tagline new)
          (aref (rmail-mime-entity-tagline entity) 2))
    (setf (rmail-mime-display-body new)
          (aref (rmail-mime-entity-body entity) 2)))
  (dolist (child (rmail-mime-entity-children entity))
    (rmail-mime-shown-mode child)))

(defun rmail-mime-hidden-mode (entity)
  "Make MIME-entity ENTITY display in hidden mode."
  (let ((new (aref (rmail-mime-entity-display entity) 1)))
    (setf (rmail-mime-display-header  new) nil)
    (setf (rmail-mime-display-tagline new) t)
    (setf (rmail-mime-display-body    new) nil))
  (dolist (child (rmail-mime-entity-children entity))
    (rmail-mime-hidden-mode child)))

(defun rmail-mime-raw-mode (entity)
  "Make MIME-entity ENTITY display in raw mode."
  (let ((new (aref (rmail-mime-entity-display entity) 1)))
    (setf (rmail-mime-display-header  new) 'raw)
    (setf (rmail-mime-display-tagline new) nil)
    (setf (rmail-mime-display-body    new) 'raw))
  (dolist (child (rmail-mime-entity-children entity))
    (rmail-mime-raw-mode child)))

(defun rmail-mime-toggle-raw (&optional state)
  "Toggle on and off the raw display mode of MIME-entity at point.
With optional argument STATE, force the specified display mode.
Use `raw' for raw mode, and any other non-nil value for decoded mode."
  (let* ((pos (if (eobp) (1- (point-max)) (point)))
	 (entity (get-text-property pos 'rmail-mime-entity))
	 (current (aref (rmail-mime-entity-display entity) 0))
	 (segment (rmail-mime-entity-segment pos entity)))
    (if (or (eq state 'raw)
	    (not (or state
	             (eq (rmail-mime-display-header current) 'raw))))
	;; Enter the raw mode.
	(rmail-mime-raw-mode entity)
      ;; Enter the shown mode.
      (rmail-mime-shown-mode entity)
      (let ((inhibit-read-only t)
	    (modified (buffer-modified-p)))
	(save-excursion
	  (goto-char (aref segment 1))
	  (rmail-mime-insert entity)
	  (restore-buffer-modified-p modified))))))

(defun rmail-mime-toggle-hidden ()
  "Hide or show the body of the MIME-entity at point."
  (interactive)
  (when (rmail-mime-message-p)
    (let* ((rmail-mime-mbox-buffer rmail-view-buffer)
	   (rmail-mime-view-buffer (current-buffer))
	   (pos (if (eobp) (1- (point-max)) (point)))
	   (entity (get-text-property pos 'rmail-mime-entity))
	   (current (aref (rmail-mime-entity-display entity) 0))
	   (segment (rmail-mime-entity-segment pos entity)))
      (if (rmail-mime-display-body current)
	  ;; Enter the hidden mode.
	  (progn
	    ;; If point is in the body part, move it to the tagline
	    ;; (or the header if tagline is not displayed).
	    (if (= (aref segment 0) 3)
		(goto-char (aref segment 2)))
	    (rmail-mime-hidden-mode entity)
	    ;; If the current entity is the topmost one, display the
	    ;; header.
	    (if (and rmail-mime-mbox-buffer (= (aref segment 1) (point-min)))
		(let ((new (aref (rmail-mime-entity-display entity) 1)))
		  (setf (rmail-mime-display-header new) t))))
	;; Query as a warning before showing if truncated.
	(if (and (not (stringp entity))
		 (rmail-mime-entity-truncated entity))
	    (unless (y-or-n-p "This entity is truncated; show anyway? ")
	      (error "Aborted")))
	;; Enter the shown mode.
	(rmail-mime-shown-mode entity)
	;; Force this body shown.
	(let ((new (aref (rmail-mime-entity-display entity) 1)))
	  (setf (rmail-mime-display-body new) t)))
      (let ((inhibit-read-only t)
	    (modified (buffer-modified-p))
	    (rmail-mime-mbox-buffer rmail-view-buffer)
	    (rmail-mime-view-buffer rmail-buffer))
	(save-excursion
	  (goto-char (aref segment 1))
	  (rmail-mime-insert entity)
	  (restore-buffer-modified-p modified))))))

(define-key rmail-mode-map "\t" #'forward-button)
(define-key rmail-mode-map [backtab] #'backward-button)
(define-key rmail-mode-map "\r" #'rmail-mime-toggle-hidden)

;;; Handlers

(defun rmail-mime-insert-tagline (entity &rest item-list)
  "Insert a tag line for MIME-entity ENTITY.
ITEM-LIST is a list of strings or button-elements (list) to add
to the tag line."
  ;; Precede the tagline by an empty line to make it a separate
  ;; paragraph, so that it is aligned to the left margin of the window
  ;; even if preceded by a right-to-left paragraph.
  (insert "\n[")
  (let ((tag (aref (rmail-mime-entity-tagline entity) 0)))
    (if (> (length tag) 0) (insert (substring tag 1) ":")))
  (insert (car (rmail-mime-entity-type entity)) " ")
  (insert-button (let ((new (aref (rmail-mime-entity-display entity) 1)))
		   (if (rmail-mime-display-body new) "Hide" "Show"))
		 :type 'rmail-mime-toggle
		 'help-echo "mouse-2, RET: Toggle show/hide")
  (dolist (item item-list)
    (when item
      (if (stringp item)
	  (insert item)
	(apply #'insert-button item))))
  ;; Follow the tagline by an empty line to make it a separate
  ;; paragraph, so that the paragraph direction of the following text
  ;; is determined based on that text.
  (insert "]\n\n"))

(defun rmail-mime-update-tagline (entity)
  "Update the current tag line for MIME-entity ENTITY."
  (let ((inhibit-read-only t)
	(modified (buffer-modified-p))
	;; If we are going to show the body, the new button label is
	;; "Hide".  Otherwise, it's "Show".
	(label
	 (if (rmail-mime-display-body
	      (aref (rmail-mime-entity-display entity) 1))
	     "Hide" "Show"))
	(button (next-button (point))))
    ;; Go to the second character of the button "Show" or "Hide".
    (goto-char (1+ (button-start button)))
    (setq button (button-at (point)))
    (save-excursion
      (insert label)
      (delete-region (point) (button-end button)))
    (delete-region (button-start button) (point))
    (put-text-property (point) (button-end button) 'rmail-mime-entity entity)
    (restore-buffer-modified-p modified)
    ;; The following call to forward-line must be in sync with how
    ;; rmail-mime-insert-tagline formats the tagline.
    (forward-line 2)))

(defun rmail-mime-insert-header (header)
  "Decode and insert a MIME-entity header HEADER in the current buffer.
HEADER is a vector [BEG END DEFAULT-STATUS].
See `rmail-mime-entity' for details."
  (let ((pos (point))
	(last-coding-system-used nil))
    (save-restriction
      (narrow-to-region pos pos)
      (with-current-buffer rmail-mime-mbox-buffer
	(let ((rmail-buffer rmail-mime-mbox-buffer)
	      (rmail-view-buffer rmail-mime-view-buffer))
	  (save-excursion
	    (goto-char (aref header 0))
	    (rmail-copy-headers (point) (aref header 1)))))
      (rfc2047-decode-region pos (point))
      (if (and last-coding-system-used (not rmail-mime-coding-system))
	  (setq rmail-mime-coding-system (cons last-coding-system-used nil)))
      (goto-char (point-min))
      (rmail-highlight-headers)
      (goto-char (point-max))
      (insert "\n"))))

(defun rmail-mime-find-header-encoding (header)
  "Return the last coding system used to decode HEADER.
HEADER is a header component of a MIME-entity object (see
`rmail-mime-entity')."
  (with-temp-buffer
    (let ((buf (current-buffer)))
      (with-current-buffer rmail-mime-mbox-buffer
	(let ((last-coding-system-used nil)
	      (rmail-buffer rmail-mime-mbox-buffer)
	      (rmail-view-buffer buf))
	  (save-excursion
	    (goto-char (aref header 0))
	    (rmail-copy-headers (point) (aref header 1)))))
      (rfc2047-decode-region (point-min) (point-max))
      last-coding-system-used)))

(defun rmail-mime-text-handler (content-type
				content-disposition
				content-transfer-encoding)
  "Handle the current buffer as a plain text MIME part."
  (rmail-mime-insert-text
   (rmail-mime-entity content-type content-disposition
		      content-transfer-encoding
		      (vector (rmail-mime--make-display nil nil nil)
		              (rmail-mime--make-display nil nil t))
		      (vector nil nil nil) (vector "" (cons nil nil) t)
		      (vector nil nil nil) nil #'rmail-mime-insert-text))
  t)

(defun rmail-mime-insert-decoded-text (entity)
  "Decode and insert the text body of MIME-entity ENTITY."
  (let* ((content-type (rmail-mime-entity-type entity))
	 (charset (cdr (assq 'charset (cdr content-type))))
	 (coding-system (if charset
			    (coding-system-from-name charset)))
	 (body (rmail-mime-entity-body entity))
	 (pos (point)))
    (or (and coding-system (coding-system-p coding-system))
	(setq coding-system 'undecided))
    (if (stringp (aref body 0))
	(insert (aref body 0))
      (let ((transfer-encoding (rmail-mime-entity-transfer-encoding entity)))
	(insert-buffer-substring rmail-mime-mbox-buffer
				 (aref body 0) (aref body 1))
	(cond ((string= transfer-encoding "base64")
	       (ignore-errors (base64-decode-region pos (point))))
	      ((string= transfer-encoding "quoted-printable")
	       (quoted-printable-decode-region pos (point))))))
    ;; If the text is empty, we don't have anything to decode.
    (and (/= pos (point))
         (decode-coding-region
          pos (point)
          ;; Use -dos decoding, to remove ^M characters left from base64
          ;; or rogue qp-encoded text.
          (coding-system-change-eol-conversion coding-system 1)))
    (if (and
	 (or (not rmail-mime-coding-system) (consp rmail-mime-coding-system))
	 (not (eq (coding-system-base coding-system) 'us-ascii)))
	(setq rmail-mime-coding-system coding-system))
    (or (bolp) (insert "\n"))))

(defun rmail-mime-insert-text (entity)
  "Presentation handler for a plain text MIME entity."
  (let ((current (aref (rmail-mime-entity-display entity) 0))
	(new (aref (rmail-mime-entity-display entity) 1))
	(header (rmail-mime-entity-header entity))
	;; (tagline (rmail-mime-entity-tagline entity))
	(body (rmail-mime-entity-body entity))
	(beg (point))
	(segment (rmail-mime-entity-segment (point) entity)))

    (or (integerp (aref body 0)) (markerp (aref body 0))
	(let ((data (buffer-string)))
	  (aset body 0 data)
	  (delete-region (point-min) (point-max))))

    ;; header
    (if (eq (rmail-mime-display-header current)
	    (rmail-mime-display-header new))
	(goto-char (aref segment 2))
      (if (rmail-mime-display-header current)
	  (delete-char (- (aref segment 2) (aref segment 1))))
      (if (rmail-mime-display-header new)
	  (rmail-mime-insert-header header)))
    ;; tagline
    (if (eq (rmail-mime-display-tagline current)
	    (rmail-mime-display-tagline new))
	(if (or (not (rmail-mime-display-tagline current))
		(eq (rmail-mime-display-body current)
		    (rmail-mime-display-body new)))
	    (forward-char (- (aref segment 3) (aref segment 2)))
	  (rmail-mime-update-tagline entity))
      (if (rmail-mime-display-tagline current)
	  (delete-char (- (aref segment 3) (aref segment 2))))
      (if (rmail-mime-display-tagline new)
	  (rmail-mime-insert-tagline entity)))
    ;; body
    (if (eq (rmail-mime-display-body current)
	    (rmail-mime-display-body new))
	(forward-char (- (aref segment 4) (aref segment 3)))
      (if (rmail-mime-display-body current)
	  (delete-char (- (aref segment 4) (aref segment 3))))
      (if (rmail-mime-display-body new)
	  (rmail-mime-insert-decoded-text entity)))
    (put-text-property beg (point) 'rmail-mime-entity entity)))

(defun rmail-mime-insert-image (entity)
  "Decode and insert the image body of MIME-entity ENTITY."
  (let* (;; (content-type (car (rmail-mime-entity-type entity)))
	 (bulk-data (aref (rmail-mime-entity-tagline entity) 1))
	 (body (rmail-mime-entity-body entity))
	 data)
    (if (stringp (aref body 0))
	(setq data (aref body 0))
      (let ((rmail-mime-mbox-buffer rmail-view-buffer)
	    (transfer-encoding (rmail-mime-entity-transfer-encoding entity)))
	(with-temp-buffer
	  (set-buffer-multibyte nil)
	  (setq buffer-undo-list t)
	  (insert-buffer-substring rmail-mime-mbox-buffer
				   (aref body 0) (aref body 1))
	  (cond ((string= transfer-encoding "base64")
		 (ignore-errors (base64-decode-region (point-min) (point-max))))
		((string= transfer-encoding "quoted-printable")
		 (quoted-printable-decode-region (point-min) (point-max))))
	  (setq data
		(buffer-substring-no-properties (point-min) (point-max))))))
    (insert-image (create-image data (cdr bulk-data) t))
    (insert "\n")))

(defun rmail-mime-insert-html (entity)
  "Decode, render, and insert html from MIME-entity ENTITY."
  (let ((body (rmail-mime-entity-body entity))
	(transfer-encoding (rmail-mime-entity-transfer-encoding entity))
	(charset (cdr (assq 'charset (cdr (rmail-mime-entity-type entity)))))
	(buffer (current-buffer))
	(case-fold-search t)
	coding-system)
    (if charset (setq coding-system (coding-system-from-name charset)))
    (or (and coding-system (coding-system-p coding-system))
	(setq coding-system 'undecided))
    (with-temp-buffer
      (set-buffer-multibyte nil)
      (setq buffer-undo-list t)
      (insert-buffer-substring rmail-mime-mbox-buffer
			       (aref body 0) (aref body 1))
      (cond ((string= transfer-encoding "base64")
	     (ignore-errors (base64-decode-region (point-min) (point-max))))
	    ((string= transfer-encoding "quoted-printable")
	     (quoted-printable-decode-region (point-min) (point-max))))
      ;; Some broken MUAs state the charset only in the HTML <head>,
      ;; so if we don't have a non-trivial coding-system at this
      ;; point, make one last attempt to find it there.
      (if (eq coding-system 'undecided)
	  (save-excursion
	    (goto-char (point-min))
	    (when (re-search-forward
		   "^<html><head><meta[^;]*; charset=\\([-a-zA-Z0-9]+\\)"
		   nil t)
	      (setq coding-system (coding-system-from-name (match-string 1)))
	      (or (and coding-system (coding-system-p coding-system))
		  (setq coding-system 'undecided)))
	    ;; Finally, let them manually force decoding if they know it.
	    (if (and (eq coding-system 'undecided)
		     (not (null coding-system-for-read)))
		(setq coding-system coding-system-for-read))))
      (decode-coding-region
       (point-min) (point)
       ;; Use -dos decoding, to remove ^M characters left from base64 or
       ;; rogue qp-encoded text.
       (coding-system-change-eol-conversion coding-system 1))
      (if (and
	   (or (not rmail-mime-coding-system) (consp rmail-mime-coding-system))
	   (not (eq (coding-system-base coding-system) 'us-ascii)))
	  (setq rmail-mime-coding-system coding-system))
      ;; Convert html in temporary buffer to text and insert in original buffer
      (let ((source-buffer (current-buffer)))
	(with-current-buffer buffer
	  (let ((start (point)))
	    (if rmail-mime-render-html-function
		(funcall rmail-mime-render-html-function source-buffer)
	      (insert-buffer-substring source-buffer))
	    (rmail-mime-fix-inserted-faces start)))))))

(declare-function libxml-parse-html-region "xml.c"
		  (start end &optional base-url discard-comments))

(defvar shr-inhibit-images)
(defvar shr-width)

(defun rmail-mime-render-html-shr (source-buffer)
  (let ((dom (with-current-buffer source-buffer
	       (libxml-parse-html-region (point-min) (point-max))))
	;; Image retrieval happens asynchronously, but meanwhile
	;; `rmail-swap-buffers' may have been run, leaving
	;; `shr-image-fetched' trying to insert the image in the wrong buffer.
	(shr-inhibit-images t)
	;; Bind shr-width to nil to force shr-insert-document break
	;; the lines at the window margin.  The default is
	;; fill-column, whose default value is too small, and screws
	;; up display of the quoted messages.
	shr-width)
    (shr-insert-document dom)))

(defun rmail-mime-render-html-lynx (source-buffer)
  (let ((destination-buffer (current-buffer)))
    (with-current-buffer source-buffer
      (call-process-region (point-min) (point-max)
			   "lynx" nil destination-buffer nil
			   "-stdin" "-dump" "-force_html"
			   "-dont_wrap_pre" "-width=70"))))

;; Put font-lock-face properties matching face properties on text
;; inserted, e.g., by shr, in text from START to point.
(defun rmail-mime-fix-inserted-faces (start)
  (while (< start (point))
    (let ((face (get-text-property start 'face))
	  (next (next-single-property-change
		 start 'face (current-buffer) (point))))
      (if face				; anything to do?
	  (put-text-property start next 'font-lock-face face))
      (setq start next))))

(defun rmail-mime-toggle-button (button)
  "Hide or show the body of the MIME-entity associated with BUTTON."
  (save-excursion
    (goto-char (button-start button))
    (rmail-mime-toggle-hidden)))

(define-button-type 'rmail-mime-toggle 'action 'rmail-mime-toggle-button)


(defun rmail-mime-bulk-handler (content-type
				content-disposition
				content-transfer-encoding)
  "Handle the current buffer as an attachment to download.
For images that Emacs is capable of displaying, the behavior
depends upon the value of `rmail-mime-show-images'."
  (rmail-mime-insert-bulk
   (rmail-mime-entity content-type content-disposition content-transfer-encoding
		      (vector (rmail-mime--make-display nil nil nil)
		              (rmail-mime--make-display nil t nil))
		      (vector nil nil nil) (vector "" (cons nil nil) t)
		      (vector nil nil nil) nil 'rmail-mime-insert-bulk)))

(defun rmail-mime-set-bulk-data (entity)
  "Setup the information about the attachment object for MIME-entity ENTITY.
The value is non-nil if and only if the attachment object should be shown
directly."
  (let ((content-type (car (rmail-mime-entity-type entity)))
	(size (cdr (assq 'size (cdr (rmail-mime-entity-disposition entity)))))
	(bulk-data (aref (rmail-mime-entity-tagline entity) 1))
	(body (rmail-mime-entity-body entity))
	type to-show)
    (cond (size
	   (setq size (string-to-number size)))
	  ((stringp (aref body 0))
	   (setq size (length (aref body 0))))
	  (t
	   ;; Rough estimation of the size.
	   (let ((encoding (rmail-mime-entity-transfer-encoding entity)))
	     (setq size (- (aref body 1) (aref body 0)))
	     (cond ((string= encoding "base64")
                    ;; https://en.wikipedia.org/wiki/Base64#MIME
		    (setq size (* size 0.73)))
		   ((string= encoding "quoted-printable")
                    ;; Assume most of the text is ASCII...
		    (setq size (/ (* size 5) 7)))))))

    (cond
     ((string-match "text/html" content-type)
      (setq type 'html))
     ((string-match "text/" content-type)
      (setq type 'text))
     ((string-match "image/\\(.*\\)" content-type)
      (setq type (and (fboundp 'image-supported-file-p)
                      (image-supported-file-p
		       (concat "." (match-string 1 content-type)))))
      (when (and type
                 rmail-mime-show-images
	         (not (eq rmail-mime-show-images 'button))
	         (or (not (numberp rmail-mime-show-images))
		     (< size rmail-mime-show-images)))
	(setq to-show t))))
    (setcar bulk-data size)
    (setcdr bulk-data type)
    to-show))

(defun rmail-mime-insert-bulk (entity)
  "Presentation handler for an attachment MIME entity."
  (let* ((content-type (rmail-mime-entity-type entity))
	 (content-disposition (rmail-mime-entity-disposition entity))
	 (current (aref (rmail-mime-entity-display entity) 0))
	 (new (aref (rmail-mime-entity-display entity) 1))
	 (header (rmail-mime-entity-header entity))
	 (tagline (rmail-mime-entity-tagline entity))
	 (bulk-data (aref tagline 1))
	 (body (rmail-mime-entity-body entity))
	 ;; Find the default directory for this media type.
	 (directory (or (catch 'directory
			  (dolist (entry rmail-mime-attachment-dirs-alist)
			    (when (string-match (car entry) (car content-type))
			      (dolist (dir (cdr entry))
				(when (file-directory-p dir)
				  (throw 'directory dir))))))
			"~"))
	 (filename (or (cdr (assq 'name (cdr content-type)))
		       (cdr (assq 'filename (cdr content-disposition)))
		       "noname"))
	 (units '(B kB MB GB))
	 (segment (rmail-mime-entity-segment (point) entity))
	 beg data size)

    (if (or (integerp (aref body 0)) (markerp (aref body 0)))
	(setq data entity
	      size (car bulk-data))
      (if (stringp (aref body 0))
	  (setq data (aref body 0))
	(setq data (buffer-string))
        (cl-assert (not (multibyte-string-p data)))
	(aset body 0 data)
	(rmail-mime-set-bulk-data entity)
	(delete-region (point-min) (point-max)))
      (setq size (length data)))
    (while (and (> size 1024.0) ; cribbed from gnus-agent-expire-done-message
		(cdr units))
      (setq size (/ size 1024.0)
	    units (cdr units)))

    (setq beg (point))

    ;; header
    (if (eq (rmail-mime-display-header current)
	    (rmail-mime-display-header new))
	(goto-char (aref segment 2))
      (if (rmail-mime-display-header current)
	  (delete-char (- (aref segment 2) (aref segment 1))))
      (if (rmail-mime-display-header new)
	  (rmail-mime-insert-header header)))

    ;; tagline
    (if (eq (rmail-mime-display-tagline current)
	    (rmail-mime-display-tagline new))
	(if (or (not (rmail-mime-display-tagline current))
		(eq (rmail-mime-display-body current)
		    (rmail-mime-display-body new)))
	    (forward-char (- (aref segment 3) (aref segment 2)))
	  (rmail-mime-update-tagline entity))
      (if (rmail-mime-display-tagline current)
	  (delete-char (- (aref segment 3) (aref segment 2))))
      (if (rmail-mime-display-tagline new)
	  (rmail-mime-insert-tagline
	   entity
	   " Save:"
	   (list filename
		 :type 'rmail-mime-save
		 'help-echo "mouse-2, RET: Save attachment"
		 'filename filename
		 'directory (file-name-as-directory directory)
		 'data data)
	   (format " (%.0f%s)" size (car units))
	   ;; We don't need this button because the "type" string of a
	   ;; tagline is the button to do this.
	   ;; (if (cdr bulk-data)
	   ;;     " ")
	   ;; (if (cdr bulk-data)
	   ;;     (list "Toggle show/hide"
	   ;; 	     :type 'rmail-mime-image
	   ;; 	     'help-echo "mouse-2, RET: Toggle show/hide"
	   ;; 	     'image-type (cdr bulk-data)
	   ;; 	     'image-data data))
	   )))
    ;; body
    (if (eq (rmail-mime-display-body current)
	    (rmail-mime-display-body new))
	(forward-char (- (aref segment 4) (aref segment 3)))
      (if (rmail-mime-display-body current)
	  (delete-char (- (aref segment 4) (aref segment 3))))
      (if (rmail-mime-display-body new)
	  (cond ((eq (cdr bulk-data) 'text)
		 (rmail-mime-insert-decoded-text entity))
		((eq (cdr bulk-data) 'html)
		 ;; Render HTML if display single message, but if searching
		 ;; don't render but just search HTML itself.
		 (if rmail-mime-searching
		     (rmail-mime-insert-decoded-text entity)
		   (rmail-mime-insert-html entity)))
		((cdr bulk-data)
		 (rmail-mime-insert-image entity))
		(t
		 ;; As we don't know how to display the body, just
		 ;; insert it as a text.
		 (rmail-mime-insert-decoded-text entity)))))
    (put-text-property beg (point) 'rmail-mime-entity entity)))

(defun rmail-mime-multipart-handler (content-type
				     content-disposition
				     content-transfer-encoding)
  "Handle the current buffer as a multipart MIME body.
The current buffer should be narrowed to the body.  CONTENT-TYPE,
CONTENT-DISPOSITION, and CONTENT-TRANSFER-ENCODING are the values
of the respective parsed headers.  See `rmail-mime-handle' for their
format."
  (rmail-mime-process-multipart
   content-type content-disposition content-transfer-encoding nil)
  t)

(defun rmail-mime-process-multipart (content-type
				     content-disposition
				     content-transfer-encoding
				     parse-tag)
  "Process the current buffer as a multipart MIME body.

If PARSE-TAG is nil, modify the current buffer directly for
showing the MIME body and return nil.

Otherwise, PARSE-TAG is a string indicating the depth and index
number of the entity.  In this case, parse the current buffer and
return a list of MIME-entity objects.

The other arguments are the same as `rmail-mime-multipart-handler'."
  ;; Some MUAs start boundaries with "--", while it should start
  ;; with "CRLF--", as defined by RFC 2046:
  ;;    The boundary delimiter MUST occur at the beginning of a line,
  ;;    i.e., following a CRLF, and the initial CRLF is considered to
  ;;    be attached to the boundary delimiter line rather than part
  ;;    of the preceding part.
  ;; We currently don't handle that.
  (let ((boundary (cdr (assq 'boundary content-type)))
	(subtype (cadr (split-string (car content-type) "/")))
	(index 0)
	beg end next entities truncated last)
    (unless boundary
      (rmail-mm-get-boundary-error-message
       "No boundary defined" content-type content-disposition
       content-transfer-encoding))
    (setq boundary (concat "\n--" boundary))
    ;; Hide the body before the first bodypart
    (goto-char (point-min))
    (when (and (search-forward boundary nil t)
	       (looking-at "[ \t]*\n"))
      (if parse-tag
	  (narrow-to-region (match-end 0) (point-max))
	(delete-region (point-min) (match-end 0))))

    ;; Change content-type to the proper default one for the children.
    (cond ((string-match "mixed" subtype)
	   (setq content-type '("text/plain")))
	  ((string-match "digest" subtype)
	   (setq content-type '("message/rfc822")))
	  (t
	   (setq content-type nil)))

    ;; Loop over all body parts, where beg points at the beginning of
    ;; the part and end points at the end of the part.  next points at
    ;; the beginning of the next part.  The current point is just
    ;; after the boundary tag.
    (setq beg (point-min))

    (while (or (and (search-forward boundary nil t)
		    (setq truncated nil end (match-beginning 0)))
	       ;; If the boundary does not appear at all,
	       ;; the message was truncated.
	       ;; Handle the rest of the truncated message
	       ;; (if it isn't empty) by pretending that the boundary
	       ;; appears at the end of the message.
	       ;; We use `last' to distinguish this from the more
	       ;; likely situation of there being an epilogue
	       ;; after the last boundary, which should be ignored.
	       ;; See rmailmm-test-multipart-handler for an example,
	       ;; and also bug#10101.
	       (and (not last)
		    (save-excursion
		      (skip-chars-forward "\n")
		      (> (point-max) (point)))
		    (setq truncated t end (point-max))))
      ;; If this is the last boundary according to RFC 2046, hide the
      ;; epilogue, else hide the boundary only.  Use a marker for
      ;; `next' because `rmail-mime-show' may change the buffer.
      (cond ((looking-at "--[ \t]*$")
	     (setq next (point-max-marker)
		   last t))
	    ((looking-at "[ \t]*\n")
	     (setq next (copy-marker (match-end 0) t)))
	    (truncated
	     ;; We're handling what's left of a truncated message.
	     (setq next (point-max-marker)))
	    (t
	     ;; The original code signaled an error as below, but
	     ;; this line may be a boundary of nested multipart.  So,
	     ;; we just set `next' to nil to skip this line
	     ;; (rmail-mm-get-boundary-error-message
	     ;;  "Malformed boundary" content-type content-disposition
	     ;;  content-transfer-encoding)
	     (setq next nil)))

      (when next
	(setq index (1+ index))
	;; Handle the part.
	(if parse-tag
	    (save-restriction
	      (narrow-to-region beg end)
	      (let ((child (rmail-mime-process
			    nil (format "%s/%d" parse-tag index)
			    content-type content-disposition)))
		;; Display a tagline.
		(setf (rmail-mime-display-tagline
		       (aref (rmail-mime-entity-display child) 1))
		      (aset (rmail-mime-entity-tagline child) 2 t))
		(setf (rmail-mime-entity-truncated child) truncated)
		(push child entities)))

	  (delete-region end next)
	  (save-restriction
	    (narrow-to-region beg end)
	    (rmail-mime-show)))
	(goto-char (setq beg next))))

    (when parse-tag
      (setq entities (nreverse entities))
      (if (string-match "alternative" subtype)
	  ;; Find the best entity to show, and hide all the others.
	  ;; If rmail-mime-prefer-html is set, html is best, then plain.
	  ;; If not, plain is best, then html.
	  ;; Then comes any other text part.
	  ;; If thereto of the same type, earlier entities in the message (later
	  ;; in the reverse list) are preferred.
	  (let (best best-priority)
	    (dolist (child entities)
	      (if (string= (or (car (rmail-mime-entity-disposition child))
			       (car content-disposition))
			   "inline")
		  (let ((type (car (rmail-mime-entity-type child))))
		    (if (string-match "text/" type)
			;; Consider all inline text parts
			(let ((priority
			       (cond ((string-match "text/html" type)
				      (if rmail-mime-prefer-html 1 2))
				     ((string-match "text/plain" type)
				      (if rmail-mime-prefer-html 2 1))
				     (t 3))))
			  (if (or (null best) (<= priority best-priority))
			      (setq best child
				    best-priority priority)))))))
	    (dolist (child entities)
	      (unless (eq best child)
		(aset (rmail-mime-entity-body child) 2 nil)
		(rmail-mime-hidden-mode child)))))
      entities)))

(defun rmail-mime-insert-multipart (entity)
  "Presentation handler for a multipart MIME entity."
  (let ((current (aref (rmail-mime-entity-display entity) 0))
	(new (aref (rmail-mime-entity-display entity) 1))
	(header (rmail-mime-entity-header entity))
	;; (tagline (rmail-mime-entity-tagline entity))
	;; (body (rmail-mime-entity-body entity))
	(beg (point))
	(segment (rmail-mime-entity-segment (point) entity)))
    ;; header
    (if (eq (rmail-mime-display-header current)
	    (rmail-mime-display-header new))
	(goto-char (aref segment 2))
      (if (rmail-mime-display-header current)
	  (delete-char (- (aref segment 2) (aref segment 1))))
      (if (rmail-mime-display-header new)
	  (rmail-mime-insert-header header)))
    ;; tagline
    (if (eq (rmail-mime-display-tagline current)
	    (rmail-mime-display-tagline new))
	(if (or (not (rmail-mime-display-tagline current))
		(eq (rmail-mime-display-body current)
		    (rmail-mime-display-body new)))
	    (forward-char (- (aref segment 3) (aref segment 2)))
	  (rmail-mime-update-tagline entity))
      (if (rmail-mime-display-tagline current)
	  (delete-char (- (aref segment 3) (aref segment 2))))
      (if (rmail-mime-display-tagline new)
	  (rmail-mime-insert-tagline entity)))

    (put-text-property beg (point) 'rmail-mime-entity entity)

    ;; body
    (if (eq (rmail-mime-display-body current)
	    (rmail-mime-display-body new))
	(forward-char (- (aref segment 4) (aref segment 3)))
      (dolist (child (rmail-mime-entity-children entity))
	(rmail-mime-insert child)))
    entity))

;;; Main code

(defun rmail-mime-handle (content-type
			  content-disposition
			  content-transfer-encoding)
  "Handle the current buffer as a MIME part.
The current buffer should be narrowed to the respective body, and
point should be at the beginning of the body.

CONTENT-TYPE, CONTENT-DISPOSITION, and CONTENT-TRANSFER-ENCODING
are the values of the respective parsed headers.  The latter should
be lower-case.  The parsed headers for CONTENT-TYPE and CONTENT-DISPOSITION
have the form

  (VALUE . ALIST)

In other words:

  (VALUE (ATTRIBUTE . VALUE) (ATTRIBUTE . VALUE) ...)

VALUE is a string and ATTRIBUTE is a symbol.

Consider the following header, for example:

Content-Type: multipart/mixed;
	boundary=\"----=_NextPart_000_0104_01C617E4.BDEC4C40\"

The parsed header value:

\(\"multipart/mixed\"
  (\"boundary\" . \"----=_NextPart_000_0104_01C617E4.BDEC4C40\"))"
  ;; Handle the content transfer encodings we know.  Unknown transfer
  ;; encodings will be passed on to the various handlers.
  (cond ((string= content-transfer-encoding "base64")
	 (when (ignore-errors
		 (base64-decode-region (point) (point-max)))
	   (setq content-transfer-encoding nil)))
	((string= content-transfer-encoding "quoted-printable")
	 (quoted-printable-decode-region (point) (point-max))
	 (setq content-transfer-encoding nil))
	((string= content-transfer-encoding "8bit")
	 ;; FIXME: Is this the correct way?
         ;; No, of course not, it just means there's no decoding to do.
	 ;; (set-buffer-multibyte nil)
         (setq content-transfer-encoding nil)
         ))
  ;; Inline stuff requires work.  Attachments are handled by the bulk
  ;; handler.
  (if (string= "inline" (car content-disposition))
      (let ((stop nil))
	(dolist (entry rmail-mime-media-type-handlers-alist)
	  (when (and (string-match (car entry) (car content-type)) (not stop))
	    (progn
	      (setq stop (funcall (cadr entry) content-type
				  content-disposition
				  content-transfer-encoding))))))
    ;; Everything else is an attachment.
    (rmail-mime-bulk-handler content-type
		       content-disposition
		       content-transfer-encoding))
  (save-restriction
    (widen)
    (let ((entity (get-text-property (1- (point)) 'rmail-mime-entity)))
      (when entity
	(let ((new (aref (rmail-mime-entity-display entity) 1)))
	  (setf (aref (rmail-mime-entity-display entity) 0)
	        (rmail-mime--copy-display new)))))))

(defun rmail-mime-show (&optional show-headers)
  "Handle the current buffer as a MIME message.
If SHOW-HEADERS is non-nil, then the headers of the current part
will shown as usual for a MIME message.  The headers are also
shown for the content type message/rfc822.  This function will be
called recursively if multiple parts are available.

The current buffer must contain a single message.  It will be
modified."
  (rmail-mime-process show-headers nil))

(defun rmail-mime-process (show-headers parse-tag &optional
					default-content-type
					default-content-disposition)
  (let ((end (point-min))
	content-type
	content-transfer-encoding
	content-disposition)
    ;; `point-min' returns the beginning and `end' points at the end
    ;; of the headers.
    (goto-char (point-min))
    ;; If we're showing a part without headers, then it will start
    ;; with a newline.
    (if (eq (char-after) ?\n)
	(setq end (1+ (point)))
      (when (search-forward "\n\n" nil t)
	(setq end (match-end 0))
	(save-restriction
	  (narrow-to-region (point-min) end)
	  ;; FIXME: Default disposition of the multipart entities should
	  ;; be inherited.
	  (setq content-type
		(mail-fetch-field "Content-Type")
		content-transfer-encoding
		(mail-fetch-field "Content-Transfer-Encoding")
		content-disposition
		(mail-fetch-field "Content-Disposition")))))
    ;; Per RFC 2045, C-T-E is case insensitive (bug#5070), but the others
    ;; are not completely so.  Hopefully mail-header-parse-* DTRT.
    (if content-transfer-encoding
	(setq content-transfer-encoding (downcase content-transfer-encoding)))
    (setq content-type
	  (if content-type
	      (or (mail-header-parse-content-type content-type)
		  '("text/plain"))
	    (or default-content-type '("text/plain"))))
    (setq content-disposition
	  (if content-disposition
	      (mail-header-parse-content-disposition content-disposition)
	    ;; If none specified, we are free to choose what we deem
	    ;; suitable according to RFC 2183.  We like inline.
	    (or default-content-disposition '("inline"))))
    ;; Unrecognized disposition types are to be treated like
    ;; attachment according to RFC 2183.
    (unless (member (car content-disposition) '("inline" "attachment"))
      (setq content-disposition '("attachment")))

    (if parse-tag
	(let* ((is-inline (string= (car content-disposition) "inline"))
	       (hdr-end (copy-marker end))
	       (header (vector (point-min-marker) hdr-end nil))
	       (tagline (vector parse-tag (cons nil nil) t))
	       (body (vector hdr-end (point-max-marker) is-inline))
	       (new (rmail-mime--make-display
	             (aref header 2) (aref tagline 2) (aref body 2)))
	       children handler entity)
	  (cond ((string-match "multipart/.*" (car content-type))
		 (save-restriction
		   (narrow-to-region (1- end) (point-max))
		   (if (zerop (length parse-tag)) ; top level of message
		       (setf (rmail-mime-display-tagline new)
		             (aset tagline 2 nil))) ; don't show tagline
		   (setq children (rmail-mime-process-multipart
				   content-type
				   content-disposition
				   content-transfer-encoding
				   parse-tag)
			 handler 'rmail-mime-insert-multipart)))
		((string-match "message/rfc822" (car content-type))
		 (save-restriction
		   (narrow-to-region end (point-max))
		   (let* ((msg (rmail-mime-process t parse-tag
						   '("text/plain") '("inline")))
			  (msg-new (aref (rmail-mime-entity-display msg) 1)))
		     ;; Show header of the child.
		     (setf (rmail-mime-display-header msg-new) t)
		     (aset (rmail-mime-entity-header msg) 2 t)
		     ;; Hide tagline of the child.
		     (setf (rmail-mime-display-tagline msg-new) nil)
		     (aset (rmail-mime-entity-tagline msg) 2 nil)
		     (setq children (list msg)
			   handler 'rmail-mime-insert-multipart))))
		((and is-inline (string-match "text/html" (car content-type)))
		 ;; Display tagline, so part can be detached
		 (setf (rmail-mime-display-tagline new) (aset tagline 2 t))
		 (setf (rmail-mime-display-body new) (aset body 2 t)) ; display body also.
		 (setq handler 'rmail-mime-insert-bulk))
		;; Inline non-HTML text
		((and is-inline (string-match "text/" (car content-type)))
		 ;; Don't need a tagline.
		 (setf (rmail-mime-display-tagline new) (aset tagline 2 nil))
		 (setq handler 'rmail-mime-insert-text))
		(t
		 ;; Force hidden mode.
		 (setf (rmail-mime-display-tagline new) (aset tagline 2 t))
		 (setf (rmail-mime-display-body new) (aset body 2 nil))
		 (setq handler 'rmail-mime-insert-bulk)))
	  (setq entity (rmail-mime-entity
			content-type
			content-disposition
			content-transfer-encoding
			(vector (rmail-mime--make-display nil nil nil) new)
			header tagline body children handler))
	  (if (and (eq handler 'rmail-mime-insert-bulk)
		   (rmail-mime-set-bulk-data entity))
	      ;; Show the body.
	      (setf (rmail-mime-display-body new) (aset body 2 t)))
	  entity)

      ;; Hide headers and handle the part.
      (put-text-property (point-min) (point-max) 'rmail-mime-entity
			 (rmail-mime-entity
			  content-type content-disposition
			  content-transfer-encoding
			  (vector (vector 'raw nil 'raw) (vector 'raw nil 'raw))
			  (vector nil nil 'raw) (vector "" (cons nil nil) nil)
			  (vector nil nil 'raw) nil nil))
      (save-restriction
	(cond ((string= (car content-type) "message/rfc822")
	       (narrow-to-region end (point-max)))
	      ((not show-headers)
	       (delete-region (point-min) end)))
	(rmail-mime-handle content-type content-disposition
			   content-transfer-encoding)))))

(defun rmail-mime-parse ()
  "Parse the current Rmail message as a MIME message.
The value is a MIME-entity object (see `rmail-mime-entity').
If an error occurs, return an error message string."
  (let ((rmail-mime-mbox-buffer (if (rmail-buffers-swapped-p)
				    rmail-view-buffer
				  (current-buffer))))
    (condition-case err
	(with-current-buffer rmail-mime-mbox-buffer
	  (save-excursion
	    (goto-char (point-min))
	    (let* ((entity (rmail-mime-process t ""
					       '("text/plain") '("inline")))
		   (new (aref (rmail-mime-entity-display entity) 1)))
	      ;; Show header.
	      (setf (rmail-mime-display-header new)
	            (aset (rmail-mime-entity-header entity) 2 t))
	      entity)))
      (error (format "%s" err)))))

(defun rmail-mime-insert (entity)
  "Insert a MIME-entity ENTITY in the current buffer.

This function will be called recursively if multiple parts are
available."
  (let ((current (aref (rmail-mime-entity-display entity) 0))
	(new (aref (rmail-mime-entity-display entity) 1)))
    (if (not (eq (rmail-mime-display-header new) 'raw))
	;; Not a raw-mode.  Each handler should handle it.
	(funcall (rmail-mime-entity-handler entity) entity)
      (let ((header (rmail-mime-entity-header entity))
	    ;; (tagline (rmail-mime-entity-tagline entity))
	    (body (rmail-mime-entity-body entity))
	    (beg (point))
	    (segment (rmail-mime-entity-segment (point) entity)))
	;; header
	(if (eq (rmail-mime-display-header current)
		(rmail-mime-display-header new))
	    (goto-char (aref segment 2))
	  (if (rmail-mime-display-header current)
	      (delete-char (- (aref segment 2) (aref segment 1))))
	  (insert-buffer-substring rmail-mime-mbox-buffer
				   (aref header 0) (aref header 1)))
	;; tagline
	(if (rmail-mime-display-tagline current)
	    (delete-char (- (aref segment 3) (aref segment 2))))
	;; body
	(let ((children (rmail-mime-entity-children entity)))
	  (if children
	      (progn
		(put-text-property beg (point) 'rmail-mime-entity entity)
		(dolist (child children)
		  (rmail-mime-insert child)))
	    (if (eq (rmail-mime-display-body current)
		    (rmail-mime-display-body new))
		(forward-char (- (aref segment 4) (aref segment 3)))
	      (if (rmail-mime-display-body current)
		(delete-char (- (aref segment 4) (aref segment 3))))
	      (insert-buffer-substring rmail-mime-mbox-buffer
				       (aref body 0) (aref body 1))
	      (or (bolp) (insert "\n")))
	    (put-text-property beg (point) 'rmail-mime-entity entity)))))
    (setf (aref (rmail-mime-entity-display entity) 0)
          (rmail-mime--copy-display new))))

(define-derived-mode rmail-mime-mode fundamental-mode "RMIME"
  "Major mode used in `rmail-mime' buffers."
  (setq font-lock-defaults '(rmail-font-lock-keywords t t nil nil)))

;;;###autoload
(defun rmail-mime (&optional _arg state)
  "Toggle the display of a MIME message.

The actual behavior depends on the value of `rmail-enable-mime'.

If `rmail-enable-mime' is non-nil (the default), this command toggles
the display of a MIME message between decoded presentation form and
raw data.  With optional prefix argument ARG, it toggles the display only
of the MIME entity at point, if there is one.  The optional argument
STATE forces a particular display state, rather than toggling.
`raw' forces raw mode, any other non-nil value forces decoded mode.

If `rmail-enable-mime' is nil, this creates a temporary \"*RMAIL*\"
buffer holding a decoded copy of the message.  Inline content-types
are handled according to `rmail-mime-media-type-handlers-alist'.
By default, this displays text and multipart messages, and offers to
download attachments as specified by `rmail-mime-attachment-dirs-alist'.
The arguments ARG and STATE have no effect in this case."
  (interactive)
  (if rmail-enable-mime
      (with-current-buffer rmail-buffer
	(if (or (rmail-mime-message-p)
		(get-text-property (point-min) 'rmail-mime-hidden))
	    (let* ((hidden (get-text-property (point-min) 'rmail-mime-hidden))
		   (desired-hidden (if state (eq state 'raw) (not hidden))))
	      (unless (eq hidden desired-hidden)
		(if (not desired-hidden)
		    (rmail-show-message rmail-current-message)
		  (let ((rmail-enable-mime nil)
			(inhibit-read-only t))
		    (rmail-show-message rmail-current-message)
		    (add-text-properties (point-min) (point-max) '(rmail-mime-hidden t))))))
	  (message "Not a MIME message, just toggling headers")
	  (rmail-toggle-header)))
    (let* ((data (rmail-apply-in-message rmail-current-message 'buffer-string))
	   (buf (get-buffer-create "*RMAIL*"))
	   (rmail-mime-mbox-buffer rmail-view-buffer)
	   (rmail-mime-view-buffer buf))
      (set-buffer buf)
      (setq buffer-undo-list t)
      (let ((inhibit-read-only t))
	;; Decoding the message in fundamental mode for speed, only
	;; switching to rmail-mime-mode at the end for display.  Eg
	;; quoted-printable-decode-region gets very slow otherwise (Bug#4993).
	(fundamental-mode)
	(erase-buffer)
	(insert data)
	(rmail-mime-show t)
	(rmail-mime-mode)
	(set-buffer-modified-p nil))
      (view-buffer buf))))

(defun rmail-mm-get-boundary-error-message (message type disposition encoding)
  "Return MESSAGE with more information on the main MIME components."
  (error "%s; type: %s; disposition: %s; encoding: %s"
	 message type disposition encoding))

(defun rmail-show-mime ()
  "Function to use for the value of `rmail-show-mime-function'."
  (let ((entity (rmail-mime-parse))
	(rmail-mime-mbox-buffer rmail-buffer)
	(rmail-mime-view-buffer rmail-view-buffer)
	(rmail-mime-coding-system nil))
    ;; If ENTITY is not a vector, it is a string describing an error.
    (if (rmail-mime-entity-p entity)
	(with-current-buffer rmail-mime-view-buffer
	  (erase-buffer)
	  ;; This condition-case is for catching an error in the
	  ;; internal MIME decoding (e.g. incorrect BASE64 form) that
	  ;; may be signaled by rmail-mime-insert.
	  ;; FIXME: The current code doesn't set a proper error symbol
	  ;; in ERR.  We must find a way to propagate a correct error
	  ;; symbol that is caused in the very deep code of text
	  ;; decoding (e.g. an error by base64-decode-region called by
	  ;; post-read-conversion function of utf-7).
	  (condition-case err
	      (progn
		(rmail-mime-insert entity)
		(if (consp rmail-mime-coding-system)
		    ;; Decoding is done by rfc2047-decode-region only for a
		    ;; header.  But, as the used coding system may have been
		    ;; overridden by mm-charset-override-alist, we can't
		    ;; trust (car rmail-mime-coding-system).  So, here we
		    ;; try the decoding again with mm-charset-override-alist
		    ;; bound to nil.
		    (let ((mm-charset-override-alist nil))
		      (setq rmail-mime-coding-system
			    (rmail-mime-find-header-encoding
			     (rmail-mime-entity-header entity)))))
		(set-buffer-file-coding-system
		 (if rmail-mime-coding-system
		     (coding-system-base rmail-mime-coding-system)
		   'undecided)
		 t t))
	    (error (setq entity (format "%s" err))))))
    ;; Re-check ENTITY.  It may be set to an error string.
    (when (stringp entity)
      ;; Decoding failed.  ENTITY is an error message.  Insert the
      ;; original message body as is, and show warning.
      (let ((region (with-current-buffer rmail-mime-mbox-buffer
		      (goto-char (point-min))
		      (re-search-forward "^$" nil t)
		      (forward-line 1)
		      (vector (point-min) (point) (point-max)))))
	(with-current-buffer rmail-mime-view-buffer
	  (let ((inhibit-read-only t))
	    (erase-buffer)
	    (rmail-mime-insert-header region)
	    (insert-buffer-substring rmail-mime-mbox-buffer
				     (aref region 1) (aref region 2))))
	(set-buffer-file-coding-system 'no-conversion t t)
	(message "MIME decoding failed: %s" entity)))))

(setq rmail-show-mime-function 'rmail-show-mime)

(defun rmail-insert-mime-forwarded-message (forward-buffer)
  "Insert the message in FORWARD-BUFFER as a forwarded message.
This is the usual value of `rmail-insert-mime-forwarded-message-function'."
  (let (contents-buffer start end)
    (with-current-buffer forward-buffer
      (setq contents-buffer
	    (if rmail-buffer-swapped
		rmail-view-buffer
	      forward-buffer)
	    start (rmail-msgbeg rmail-current-message)
	    end (rmail-msgend rmail-current-message)))
    (message-forward-make-body-mime contents-buffer start end)))

(setq rmail-insert-mime-forwarded-message-function
      'rmail-insert-mime-forwarded-message)

(defun rmail-insert-mime-resent-message (forward-buffer)
  "Function to set in `rmail-insert-mime-resent-message-function' (which see)."
  (insert-buffer-substring
   (with-current-buffer forward-buffer rmail-view-buffer))
  (goto-char (point-min))
  (when (looking-at "From ")
    (forward-line 1)
    (delete-region (point-min) (point))))

(setq rmail-insert-mime-resent-message-function
      'rmail-insert-mime-resent-message)

(defun rmail-search-mime-message (msg regexp)
  "Function to set in `rmail-search-mime-message-function' (which see)."
  (save-restriction
    (narrow-to-region (rmail-msgbeg msg) (rmail-msgend msg))
    (let* ((rmail-mime-searching t)	; mark inside search
	   (rmail-mime-mbox-buffer (current-buffer))
	   (rmail-mime-view-buffer rmail-view-buffer)
	   (header-end (save-excursion
			 (re-search-forward "^$" nil 'move) (point)))
	   ;; (body-end (point-max))
	   (entity (rmail-mime-parse)))
      (or
       ;; At first, just search the headers.
       (with-temp-buffer
	 (insert-buffer-substring rmail-mime-mbox-buffer nil header-end)
	 (rfc2047-decode-region (point-min) (point))
	 (goto-char (point-min))
	 (re-search-forward regexp nil t))
       ;; Next, search the body.
       (if (and entity
		;; RMS: I am not sure why, but sometimes this is a string.
		(not (stringp entity))
		(let* ((content-type (rmail-mime-entity-type entity))
		       (charset (cdr (assq 'charset (cdr content-type)))))
		  (or (not (string-match "text/.*" (car content-type)))
		      (and charset
			   (not (string= (downcase charset) "us-ascii"))))))
	   ;; Search the decoded MIME message.
	   (with-temp-buffer
	     (rmail-mime-insert entity)
	     (goto-char (point-min))
	     (re-search-forward regexp nil t))
	 ;; Search the body without decoding.
	 (goto-char header-end)
	 (re-search-forward regexp nil t))))))

(setq rmail-search-mime-message-function 'rmail-search-mime-message)

(provide 'rmailmm)

;;; rmailmm.el ends here
