;;; ede/locate.el --- Locate support  -*- lexical-binding: t; -*-

;; Copyright (C) 2008-2025 Free Software Foundation, Inc.

;; Author: Eric M. Ludlam <zappo@gnu.org>

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
;;
;; Support for various LOCATE type functions.
;;
;; A key feature of EDE is `ede-expand-filename', which allows a
;; project to expand a filename reference in one file to some actual
;; filename.
;;
;; In that way, you may #include <foo.h>, and without knowing how to
;; read a Makefile, find it in <root>/include/foo.h.
;;
;; Some projects are regular, such as the Emacs project.  Some
;; projects are completely controlled by EDE, such sh the Project.ede
;; based projects.
;;
;; For other projects, having a "quick hack" to support these location
;; routines is handy.
;;
;; The baseclass `ede-locate-base' provides the abstract interface to
;; finding files in a project.
;;
;; New location routines will subclass `ede-locate-base'.
;;
;; How to use:
;;
;; Configure `ede-locate-setup-options' to add the types of locate
;; features you have available.  EDE will then enable the correct one
;; when it is available.

(require 'ede)
(eval-when-compile (require 'locate))

;;; Code:
(defcustom ede-locate-setup-options
  '(ede-locate-base)
  "List of locate objects to try out by default.
Listed in order of preference.  If the first item cannot be used in
a particular project, then the next one is tried.
It is always assumed that `ede-locate-base' is at end of the list."
  :group 'ede
  :type '(repeat
	  (choice (const :tag "None" ede-locate-base)
		  (const :tag "locate" ede-locate-locate)
		  (const :tag "GNU Global" ede-locate-global)
		  (const :tag "ID Utils" ede-locate-idutils)
		  (const :tag "CScope" ede-locate-cscope)))
  )

;;;###autoload
(defun ede-enable-locate-on-project (&optional project)
  "Enable an EDE locate feature on PROJECT.
Attempt to guess which project locate style to use
based on `ede-locate-setup-options'."
  (interactive)
  (let* ((proj (or project (ede-toplevel)))
	 (root (ede-project-root-directory proj))
	 (opts ede-locate-setup-options)
	 (ans nil))
    (while (and opts (not ans))
      (when (ede-locate-ok-in-project (car opts) root)
	;; If interactive, check with the user.
	(when (or (not (called-interactively-p 'any))
		  (y-or-n-p (format "Set project locator to %s? " (car opts))))
	  (setq ans (car opts))))
      (setq opts (cdr opts)))
    ;; No match?  Always create the baseclass for the hashing tool.
    (when (not ans)
      (when (called-interactively-p 'interactive)
	(message "Setting locator to ede-locate-base"))
      (setq ans 'ede-locate-base))
    (oset proj locate-obj (make-instance ans :root root))
    (when (called-interactively-p 'interactive)
      (message "Setting locator to %s" ans))
    ))

;;; LOCATE BASECLASS
;;
;; The baseclass for all location style queries.
(defclass ede-locate-base ()
  ((root :initarg :root
	 :documentation
	 "The root of these locate searches.")
   (file :documentation
	 "The last file searched for with EDE locate.")
   (lastanswer :documentation
	      "The last answer provided by the locator.")
   (hash :documentation
	 "Hash table of previously found files.")
   )
  "Baseclass for LOCATE feature in EDE.")

(cl-defmethod initialize-instance ((loc ede-locate-base) &rest _fields)
  "Make sure we have a hash table."
  ;; Basic setup.
  (cl-call-next-method)
  ;; Make sure we have a hash table.
  (ede-locate-flush-hash loc)
  )

(cl-defmethod ede-locate-ok-in-project ((_loc (subclass ede-locate-base))
					_root)
  "Is it ok to use this project type under ROOT."
  t)

(cl-defmethod ede-locate-flush-hash ((loc ede-locate-base))
  "For LOC, flush hash table and start from scratch."
  (oset loc hash (make-hash-table :test 'equal)))

(cl-defmethod ede-locate-file-in-hash ((loc ede-locate-base)
				    filestring)
  "For LOC, is the file FILESTRING in our hash table?"
  (gethash filestring (oref loc hash)))

(cl-defmethod ede-locate-add-file-to-hash ((loc ede-locate-base)
					filestring fullfilename)
  "For LOC, add FILESTR to the hash with FULLFILENAME."
  (puthash filestring fullfilename (oref loc hash)))

(cl-defmethod ede-locate-file-in-project ((loc ede-locate-base)
				       filesubstring
				       )
  "Locate with LOC occurrences of FILESUBSTRING.
Searches are done under the current root of the EDE project
that created this EDE locate object."
  (let ((ans (ede-locate-file-in-project-impl loc filesubstring))
	)
    (oset loc file filesubstring)
    (oset loc lastanswer ans)
    ans))

(cl-defmethod ede-locate-file-in-project-impl ((_loc ede-locate-base)
					       _filesubstring)
  "Locate with LOC occurrences of FILESUBSTRING.
Searches are done under the current root of the EDE project
that created this EDE locate object."
  nil)

(cl-defmethod ede-locate-create/update-root-database
  ((loc (subclass ede-locate-base)) _root)
  "Create or update the database for the current project.
You cannot create projects for the baseclass."
  (error "Cannot create/update a database of type %S"
	 (eieio-object-name loc)))

;;; LOCATE
;;
;; Using the standard unix "locate" command.
;; Since locate is system wide, we need to hack the search
;; to restrict it to within just this project.

(defclass ede-locate-locate (ede-locate-base)
  ()
  "EDE Locator using the locate command.
Configure the Emacs `locate-command' variable to also
configure the use of EDE locate.")

(cl-defmethod ede-locate-ok-in-project ((_loc (subclass ede-locate-locate))
					_root)
  "Is it ok to use this project type under ROOT."
  (or (featurep 'locate) (locate-library "locate"))
  )

(cl-defmethod ede-locate-file-in-project-impl ((loc ede-locate-locate)
					    filesubstring)
  "Locate with LOC occurrences of FILESUBSTRING under PROJECTROOT.
Searches are done under the current root of the EDE project
that created this EDE locate object."
  ;; We want something like:
  ;;  /my/project/root*/filesubstring.c
  (let* ((searchstr (concat (directory-file-name (oref loc root))
			    "*/" filesubstring))
	 (b (get-buffer-create "*LOCATE*"))
	 (cd default-directory)
	 )
    (with-current-buffer b
      (setq default-directory cd)
      (erase-buffer))
    (apply #'call-process locate-command
	   nil b nil
	   searchstr nil)
    (with-current-buffer b
      (split-string (buffer-string) "\n" t))
    )
  )

;;; GLOBAL
;;

(declare-function cedet-gnu-global-version-check "cedet-global")
(declare-function cedet-gnu-global-root "cedet-global")
(declare-function cedet-gnu-global-expand-filename "cedet-global")
(declare-function cedet-gnu-global-create/update-database "cedet-global")

(defclass ede-locate-global (ede-locate-base)
  ()
  "EDE Locator using GNU Global.
Configure EDE's use of GNU Global through the cedet-global.el
variable `cedet-global-command'.")

(cl-defmethod initialize-instance ((loc ede-locate-global)
				   &rest _slots)
  "Make sure that we can use GNU Global."
  (require 'cedet-global)
  ;; Get ourselves initialized.
  (cl-call-next-method)
  ;; Do the checks.
  (cedet-gnu-global-version-check)
  (let* ((default-directory (oref loc root))
	 (root (cedet-gnu-global-root)))
    (when (not root)
      (error "No GNU Global project found for %s"
	     (oref loc root))))
  )

(cl-defmethod ede-locate-ok-in-project ((_loc (subclass ede-locate-global))
					root)
  "Is it ok to use this project type under ROOT."
  (require 'cedet-global)
  (cedet-gnu-global-version-check)
  (let* ((default-directory root)
	 (newroot (cedet-gnu-global-root)))
    newroot))

(cl-defmethod ede-locate-file-in-project-impl ((loc ede-locate-global)
                                               filesubstring)
  "Locate occurrences of FILESUBSTRING in LOC, using GNU Global."
  (require 'cedet-global)
  (let ((default-directory (oref loc root)))
    (cedet-gnu-global-expand-filename filesubstring)))

(cl-defmethod ede-locate-create/update-root-database
  ((_loc (subclass ede-locate-global)) root)
  "Create or update the GNU Global database for the current project."
  (cedet-gnu-global-create/update-database root))

;;; IDUTILS
;;

(declare-function cedet-idutils-version-check "cedet-idutils")
(declare-function cedet-idutils-support-for-directory "cedet-idutils")
(declare-function cedet-idutils-expand-filename "cedet-idutils")
(declare-function cedet-idutils-create/update-database "cedet-idutils")

(defclass ede-locate-idutils (ede-locate-base)
  ()
  "EDE Locator using IDUtils.
Configure EDE's use of IDUtils through the cedet-idutils.el
file name searching variable `cedet-idutils-file-command'.")

(cl-defmethod initialize-instance ((loc ede-locate-idutils)
				   &rest _slots)
  "Make sure that we can use IDUtils."
  ;; Get ourselves initialized.
  (cl-call-next-method)
  ;; Do the checks.
  (require 'cedet-idutils)
  (cedet-idutils-version-check)
  (when (not (cedet-idutils-support-for-directory (oref loc root)))
    (error "Cannot use IDUtils in %s"
	   (oref loc root)))
  )

(cl-defmethod ede-locate-ok-in-project ((_loc (subclass ede-locate-idutils))
					root)
  "Is it ok to use this project type under ROOT."
  (require 'cedet-idutils)
  (cedet-idutils-version-check)
  (when (cedet-idutils-support-for-directory root)
    root))

(cl-defmethod ede-locate-file-in-project-impl ((loc ede-locate-idutils)
					    filesubstring)
  "Locate with LOC occurrences of FILESUBSTRING under PROJECTROOT.
Searches are done under the current root of the EDE project
that created this EDE locate object."
  (require 'cedet-idutils)
  (let ((default-directory (oref loc root)))
    (cedet-idutils-expand-filename filesubstring)))

(cl-defmethod ede-locate-create/update-root-database
  ((_loc (subclass ede-locate-idutils)) root)
  "Create or update the GNU Global database for the current project."
  (cedet-idutils-create/update-database root))

;;; CSCOPE
;;

(declare-function cedet-cscope-version-check "cedet-cscope")
(declare-function cedet-cscope-support-for-directory "cedet-cscope")
(declare-function cedet-cscope-expand-filename "cedet-cscope")
(declare-function cedet-cscope-create/update-database "cedet-cscope")

(defclass ede-locate-cscope (ede-locate-base)
  ()
  "EDE Locator using Cscope.
Configure EDE's use of Cscope through the cedet-cscope.el
file name searching variable `cedet-cscope-command'.")

(cl-defmethod initialize-instance ((loc ede-locate-cscope)
				   &rest _slots)
  "Make sure that we can use Cscope."
  ;; Get ourselves initialized.
  (cl-call-next-method)
  ;; Do the checks.
  (require 'cedet-cscope)
  (cedet-cscope-version-check)
  (when (not (cedet-cscope-support-for-directory (oref loc root)))
    (error "Cannot use Cscope in %s"
	   (oref loc root)))
  )

(cl-defmethod ede-locate-ok-in-project ((_loc (subclass ede-locate-cscope))
					root)
  "Is it ok to use this project type under ROOT."
  (require 'cedet-cscope)
  (cedet-cscope-version-check)
  (when (cedet-cscope-support-for-directory root)
    root))

(cl-defmethod ede-locate-file-in-project-impl ((loc ede-locate-cscope)
					    filesubstring)
  "Locate with LOC occurrences of FILESUBSTRING under PROJECTROOT.
Searches are done under the current root of the EDE project
that created this EDE locate object."
  (let ((default-directory (oref loc root)))
    (require 'cedet-cscope)
    (cedet-cscope-expand-filename filesubstring)))

(cl-defmethod ede-locate-create/update-root-database
  ((_loc (subclass ede-locate-cscope)) root)
  "Create or update the Cscope database for the current project."
  (require 'cedet-cscope)
  (cedet-cscope-create/update-database root))

(provide 'ede/locate)

;; Local variables:
;; generated-autoload-file: "loaddefs.el"
;; generated-autoload-load-name: "ede/locate"
;; End:

;;; ede/locate.el ends here
