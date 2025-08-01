;;; vc-tests.el --- Tests of different backends of vc.el  -*- lexical-binding:t -*-

;; Copyright (C) 2014-2025 Free Software Foundation, Inc.

;; Author: Michael Albinus <michael.albinus@gmx.de>

;; This file is part of GNU Emacs.
;;
;; GNU Emacs is free software: you can redistribute it and/or
;; modify it under the terms of the GNU General Public License as
;; published by the Free Software Foundation, either version 3 of the
;; License, or (at your option) any later version.
;;
;; GNU Emacs is distributed in the hope that it will be useful, but
;; WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
;; General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs.  If not, see <https://www.gnu.org/licenses/>.

;;; Commentary:

;; For every supported VC on the machine, different test cases are
;; generated automatically.

;; Functions to be tested (see Commentary of vc.el).  Mandatory
;; functions are marked with `*', optional functions are marked with `-':

;; BACKEND PROPERTIES
;;
;; * revision-granularity                                       DONE

;; STATE-QUERYING FUNCTIONS
;;
;; * registered (file)                                          DONE
;; * state (file)                                               DONE
;; - dir-status (dir update-function)
;; - dir-status-files (dir files default-state update-function)
;; - dir-extra-headers (dir)
;; - dir-printer (fileinfo)
;; - status-fileinfo-extra (file)
;; * working-revision (file)                                    DONE
;; - latest-on-branch-p (file)
;; * checkout-model (files)                                     DONE
;; - mode-line-string (file)
;; - other-working-trees ()                                     DONE

;; STATE-CHANGING FUNCTIONS
;;
;; * create-repo (backend)                                      DONE
;; * register (files &optional comment)                         DONE
;; - responsible-p (file)
;; - receive-file (file rev)
;; - unregister (file)                                          DONE
;; * checkin (files comment)                                    DONE
;; * find-revision (file rev buffer)
;; * checkout (file &optional rev)
;; * revert (file &optional contents-done)
;; - rollback (files)
;; - merge-file (file rev1 rev2)
;; - merge-branch ()
;; - merge-news (file)
;; - pull (prompt)
;; - steal-lock (file &optional revision)
;; - modify-change-comment (files rev comment)
;; - mark-resolved (files)
;; - find-admin-dir (file)
;; - add-working-tree (directory)                               DONE
;; - delete-working-tree (directory)                            DONE
;; - move-working-tree (from to)                                DONE

;; HISTORY FUNCTIONS
;;
;; * print-log (files buffer &optional shortlog start-revision limit)
;; - log-outgoing (backend remote-location)
;; - log-incoming (backend remote-location)
;; - log-view-mode ()
;; - show-log-entry (revision)
;; - comment-history (file)
;; - update-changelog (files)
;; * diff (files &optional async rev1 rev2 buffer)              DONE
;; - revision-completion-table (files)
;; - annotate-command (file buf &optional rev)
;; - annotate-time ()
;; - annotate-current-time ()
;; - annotate-extract-revision-at-line ()
;; - region-history (FILE BUFFER LFROM LTO)
;; - region-history-mode ()

;; TAG SYSTEM
;;
;; - create-tag (dir name branchp)
;; - retrieve-tag (dir name update)

;; MISCELLANEOUS
;;
;; - make-version-backups-p (file)
;; - root (file)
;; - ignore (file &optional directory)
;; - ignore-completion-table
;; - previous-revision (file rev)
;; - next-revision (file rev)
;; - log-edit-mode ()
;; - check-headers ()
;; - delete-file (file)
;; - rename-file (old new)                                      DONE
;; - find-file-hook ()
;; - extra-menu ()
;; - extra-dir-menu ()
;; - conflicted-files (dir)

;;; Code:

(require 'ert)
(require 'ert-x)
(require 'vc)
(require 'log-edit)
(require 'project)
(require 'cl-lib)

(declare-function w32-application-type "w32proc.c")

;; The working horses.

(defvar vc-test--cleanup-hook nil
  "Functions for cleanup at the end of an ert test.
Don't set it globally, the functions should be let-bound.")

(defun vc-test--revision-granularity-function (backend)
  "Run the `revision-granularity' backend function."
  (vc-call-backend backend 'revision-granularity))

(defun vc-test--create-repo-function (backend)
  "Run the `vc-create-repo' backend function.
For backends which don't support it, it is emulated."

  (cond
   ((eq backend 'CVS)
    (let ((tmp-dir
	   (expand-file-name
	    (make-temp-name "vc-test") temporary-file-directory)))
      (make-directory (expand-file-name "module" tmp-dir) 'parents)
      (make-directory (expand-file-name "CVSROOT" tmp-dir) 'parents)
      (if (not (fboundp 'w32-application-type))
          (shell-command-to-string (format "cvs -Q -d:local:%s co module"
                                           tmp-dir))
        (let ((cvs-prog (executable-find "cvs"))
              (tdir tmp-dir))
          ;; If CVS executable is an MSYS program, reformat the file
          ;; name of TMP-DIR to have the /d/foo/bar form supported by
          ;; MSYS programs.  (FIXME: What about Cygwin cvs.exe?)
          (if (eq (w32-application-type cvs-prog) 'msys)
              (setq tdir
                    (concat "/" (substring tmp-dir 0 1) (substring tmp-dir 2))))
          (shell-command-to-string (format "cvs -Q -d:local:%s co module"
                                           tdir))))
      (rename-file "module/CVS" default-directory)
      (delete-directory "module" 'recursive)
      ;; We must cleanup the "remote" CVS repo as well.
      (add-hook 'vc-test--cleanup-hook
		(lambda () (delete-directory tmp-dir 'recursive)))))

   ((eq backend 'Arch)
    (let ((archive-name (format "%s--%s" user-mail-address (random))))
      (when (string-match
	     "no arch user id set" (shell-command-to-string "tla my-id"))
	(shell-command-to-string
	 (format "tla my-id \"<%s>\"" user-mail-address)))
      (shell-command-to-string
       (format "tla make-archive %s %s" archive-name default-directory))
      (shell-command-to-string
       (format "tla my-default-archive %s" archive-name))))

   ((eq backend 'Mtn)
    (let ((archive-name "foo.mtn"))
      (shell-command-to-string
       (format
	"mtn db init --db=%s"
	(expand-file-name archive-name default-directory)))
      (shell-command-to-string
       (format "mtn --db=%s --branch=foo setup ." archive-name))))

   (t (vc-create-repo backend))))

(defmacro vc--fix-home-for-bzr (tempdir)
  ;; See the comment in `vc-bzr-test-bug9726'.
  `(when (eq backend 'Bzr)
     (push (format "BZR_HOME=%s" ,tempdir) process-environment)
     (push (format "HOME=%s" ,tempdir) process-environment)))

(defun vc-test--create-repo (backend)
  "Create a test repository in `default-directory', a temporary directory."
  (ert-with-temp-directory tempdir
    (let ((vc-handled-backends `(,backend))
          (default-directory
           (file-name-as-directory
            (expand-file-name
             (make-temp-name "vc-test") temporary-file-directory)))
          (process-environment process-environment)
          vc-test--cleanup-hook)
      (vc--fix-home-for-bzr tempdir)
      (unwind-protect
          (progn
            ;; Cleanup.
            (add-hook
             'vc-test--cleanup-hook
             (let ((dir default-directory))
               (lambda () (delete-directory dir 'recursive))))

            ;; Check the revision granularity.
            (should (memq (vc-test--revision-granularity-function backend)
                          '(file repository)))

            ;; Create empty repository.
            (make-directory default-directory)
            (should (file-directory-p default-directory))
            (vc-test--create-repo-function backend)
            (should (eq (vc-responsible-backend default-directory) backend)))

        ;; Save exit.
        (ignore-errors
          (run-hooks 'vc-test--cleanup-hook))))))

;; FIXME: Why isn't there `vc-unregister'?
(defun vc-test--unregister-function (backend file)
  "Run the `vc-unregister' backend function.
For backends which don't support it, `vc-not-supported' is signaled."
  ;; CVS, SVN, SCCS, SRC and Mtn are not supported, and will signal
  ;; `vc-not-supported'.
  (prog1
      (vc-call-backend backend 'unregister file)
    (vc-file-clearprops file)))

(defmacro vc-test--run-maybe-unsupported-function (func &rest args)
  "Run FUNC with ARGS as arguments.
Catch the `vc-not-supported' error."
  `(condition-case err
       (funcall ,func ,@args)
     (vc-not-supported 'vc-not-supported)
     (t (signal (car err) (cdr err)))))

(defun vc-test--register (backend)
  "Register and unregister a file.
This checks also `vc-backend' and `vc-responsible-backend'."
  (ert-with-temp-directory tempdir
    (let ((vc-handled-backends `(,backend))
          (default-directory
           (file-name-as-directory
            (expand-file-name
             (make-temp-name "vc-test") temporary-file-directory)))
          (process-environment process-environment)
          vc-test--cleanup-hook)
      (vc--fix-home-for-bzr tempdir)
      (unwind-protect
          (progn
            ;; Cleanup.
            (add-hook
             'vc-test--cleanup-hook
             (let ((dir default-directory))
               (lambda () (delete-directory dir 'recursive))))

            ;; Create empty repository.
            (make-directory default-directory)
            (vc-test--create-repo-function backend)
            ;; For file oriented backends CVS, RCS and SVN the backend is
            ;; returned, and the directory is registered already.
            (should (if (vc-backend default-directory)
                        (vc-registered default-directory)
                      (not (vc-registered default-directory))))
            (should (eq (vc-responsible-backend default-directory) backend))

            (let ((tmp-name1 (expand-file-name "foo" default-directory))
                  (tmp-name2 "bla"))
              ;; Register files.  Check for it.
              (write-region "foo" nil tmp-name1 nil 'nomessage)
              (should (file-exists-p tmp-name1))
              (should-not (vc-backend tmp-name1))
              (should (eq (vc-responsible-backend tmp-name1) backend))
              (should-not (vc-registered tmp-name1))

              (write-region "bla" nil tmp-name2 nil 'nomessage)
              (should (file-exists-p tmp-name2))
              (should-not (vc-backend tmp-name2))
              (should (eq (vc-responsible-backend tmp-name2) backend))
              (should-not (vc-registered tmp-name2))

              (vc-register (list backend (list tmp-name1 tmp-name2)))
              (should (file-exists-p tmp-name1))
              (should (eq (vc-backend tmp-name1) backend))
              (should (eq (vc-responsible-backend tmp-name1) backend))
              (should (vc-registered tmp-name1))

              (should (file-exists-p tmp-name2))
              (should (eq (vc-backend tmp-name2) backend))
              (should (eq (vc-responsible-backend tmp-name2) backend))
              (should (vc-registered tmp-name2))

              ;; `vc-backend' accepts also a list of files,
              ;; `vc-responsible-backend' doesn't.
              (should (vc-backend (list tmp-name1 tmp-name2)))

              ;; Unregister the files.
              (unless (eq (vc-test--run-maybe-unsupported-function
                           'vc-test--unregister-function backend tmp-name1)
                          'vc-not-supported)
                (should-not (vc-backend tmp-name1))
                (should-not (vc-registered tmp-name1)))
              (unless (eq (vc-test--run-maybe-unsupported-function
                           'vc-test--unregister-function backend tmp-name2)
                          'vc-not-supported)
                (should-not (vc-backend tmp-name2))
                (should-not (vc-registered tmp-name2)))

              ;; The files should still exist.
              (should (file-exists-p tmp-name1))
              (should (file-exists-p tmp-name2))))

        ;; Save exit.
        (ignore-errors
          (run-hooks 'vc-test--cleanup-hook))))))

(defun vc-test--state (backend)
  "Check the different states of a file."
  (ert-with-temp-directory tempdir
    (let ((vc-handled-backends `(,backend))
          (default-directory
           (file-name-as-directory
            (expand-file-name
             (make-temp-name "vc-test") temporary-file-directory)))
          (process-environment process-environment)
          vc-test--cleanup-hook)
      (vc--fix-home-for-bzr tempdir)
      (unwind-protect
          (progn
            ;; Cleanup.
            (add-hook
             'vc-test--cleanup-hook
             (let ((dir default-directory))
               (lambda () (delete-directory dir 'recursive))))

            ;; Create empty repository.
            (make-directory default-directory)
            (vc-test--create-repo-function backend)

            (let ((tmp-name (expand-file-name "foo" default-directory)))
              ;; Check state of a nonexistent file.

              (message "vc-state2 %s" (vc-state tmp-name))
              (should (null (vc-state tmp-name)))

              ;; Write a new file.  Check state.
              (write-region "foo" nil tmp-name nil 'nomessage)

              ;; nil: Mtn
              ;; unregistered: Bzr CVS Git Hg SVN RCS
              (message "vc-state3 %s %s" backend (vc-state tmp-name backend))
              (should (memq (vc-state tmp-name backend) '(nil unregistered)))

              ;; Register a file.  Check state.
              (vc-register
               (list backend (list (file-name-nondirectory tmp-name))))

              ;; FIXME: nil is definitely wrong.
              ;; nil: SRC
              ;; added: Bzr CVS Git Hg Mtn SVN
              ;; up-to-date: RCS SCCS
              (message "vc-state4 %s" (vc-state tmp-name))
              (should (memq (vc-state tmp-name) '(nil added up-to-date)))

              ;; Unregister the file.  Check state.
              (if (eq (vc-test--run-maybe-unsupported-function
                       'vc-test--unregister-function backend tmp-name)
                      'vc-not-supported)
                  (message "vc-state5 unsupported")
                ;; unregistered: Bzr Git RCS Hg
                ;; unsupported: CVS SCCS SRC SVN
                (message "vc-state5 %s %s" backend (vc-state tmp-name backend))
                (should (memq (vc-state tmp-name backend)
                              '(nil unregistered))))))

        ;; Save exit.
        (ignore-errors
          (run-hooks 'vc-test--cleanup-hook))))))

(defun vc-test--working-revision (backend)
  "Check the working revision of a repository."
  (ert-with-temp-directory tempdir
    (let ((vc-handled-backends `(,backend))
          (default-directory
           (file-name-as-directory
            (expand-file-name
             (make-temp-name "vc-test") temporary-file-directory)))
          (process-environment process-environment)
          vc-test--cleanup-hook)
      (vc--fix-home-for-bzr tempdir)
      (unwind-protect
          (progn
            ;; Cleanup.
            (add-hook
             'vc-test--cleanup-hook
             (let ((dir default-directory))
               (lambda () (delete-directory dir 'recursive))))

            ;; Create empty repository.  Check working revision of
            ;; repository, should be nil.
            (make-directory default-directory)
            (vc-test--create-repo-function backend)

            ;; FIXME: Is the value for SVN correct?
            ;; nil: Bzr CVS Git Hg Mtn RCS SCCS SRC
            ;; "0": SVN
            (message
             "vc-working-revision1 %s" (vc-working-revision default-directory))
            (should (member (vc-working-revision default-directory) '(nil "0")))

            (let ((tmp-name (expand-file-name "foo" default-directory)))
              ;; Check initial working revision, should be nil until
              ;; it's registered.

              (message "vc-working-revision2 %s" (vc-working-revision tmp-name))
              (should-not (vc-working-revision tmp-name))

              ;; Write a new file.  Check working revision.
              (write-region "foo" nil tmp-name nil 'nomessage)

              (message "vc-working-revision3 %s" (vc-working-revision tmp-name))
              (should-not (vc-working-revision tmp-name))

              ;; Register a file.  Check working revision.
              (vc-register
               (list backend (list (file-name-nondirectory tmp-name))))

              ;; XXX: nil is fine, at least in Git's case, because
              ;; `vc-register' only makes the file `added' in this case.
              ;; nil: Git Mtn
              ;; "0": Bzr CVS Hg SRC SVN
              ;; "1.1": RCS SCCS
              ;; "-1": Hg versions before 5 (probably)
              (message "vc-working-revision4 %s" (vc-working-revision tmp-name))
              (should (member (vc-working-revision tmp-name) '(nil "0" "1.1" "-1")))

              ;; TODO: Call `vc-checkin', and check the resulting
              ;; working revision.  None of the return values should be
              ;; nil then.

              ;; Unregister the file.  Check working revision.
              (if (eq (vc-test--run-maybe-unsupported-function
                       'vc-test--unregister-function backend tmp-name)
                      'vc-not-supported)
                  (message "vc-working-revision5 unsupported")
                ;; nil: Bzr Git Hg RCS
                ;; unsupported: CVS Mtn SCCS SRC SVN
                (message "vc-working-revision5 %s" (vc-working-revision tmp-name))
                (should-not (vc-working-revision tmp-name)))))

        ;; Save exit.
        (ignore-errors
          (run-hooks 'vc-test--cleanup-hook))))))

(defun vc-test--checkout-model (backend)
  "Check the checkout model of a repository."
  (ert-with-temp-directory tempdir
    (let ((vc-handled-backends `(,backend))
          (default-directory
           (file-name-as-directory
            (expand-file-name
             (make-temp-name "vc-test") temporary-file-directory)))
          (process-environment process-environment)
          vc-test--cleanup-hook)
      (vc--fix-home-for-bzr tempdir)
      (unwind-protect
          (progn
            ;; Cleanup.
            (add-hook
             'vc-test--cleanup-hook
             (let ((dir default-directory))
               (lambda () (delete-directory dir 'recursive))))

            ;; Create empty repository.  Check repository checkout model.
            (make-directory default-directory)
            (vc-test--create-repo-function backend)

            ;; Surprisingly, none of the backends returns 'announce.
            ;; implicit: Bzr CVS Git Hg Mtn SRC SVN
            ;; locking: RCS SCCS
            (message
             "vc-checkout-model1 %s"
             (vc-checkout-model backend default-directory))
            (should (memq (vc-checkout-model backend default-directory)
                          '(announce implicit locking)))

            (let ((tmp-name (expand-file-name "foo" default-directory)))
              ;; Check checkout model of a nonexistent file.

              ;; implicit: Bzr CVS Git Hg Mtn SRC SVN
              ;; locking: RCS SCCS
              (message
               "vc-checkout-model2 %s" (vc-checkout-model backend tmp-name))
              (should (memq (vc-checkout-model backend tmp-name)
                            '(announce implicit locking)))

              ;; Write a new file.  Check checkout model.
              (write-region "foo" nil tmp-name nil 'nomessage)

              ;; implicit: Bzr CVS Git Hg Mtn SRC SVN
              ;; locking: RCS SCCS
              (message
               "vc-checkout-model3 %s" (vc-checkout-model backend tmp-name))
              (should (memq (vc-checkout-model backend tmp-name)
                            '(announce implicit locking)))

              ;; Register a file.  Check checkout model.
              (vc-register
               (list backend (list (file-name-nondirectory tmp-name))))

              ;; implicit: Bzr CVS Git Hg Mtn SRC SVN
              ;; locking: RCS SCCS
              (message
               "vc-checkout-model4 %s" (vc-checkout-model backend tmp-name))
              (should (memq (vc-checkout-model backend tmp-name)
                            '(announce implicit locking)))

              ;; Unregister the file.  Check checkout model.
              (if (eq (vc-test--run-maybe-unsupported-function
                       'vc-test--unregister-function backend tmp-name)
                      'vc-not-supported)
                  (message "vc-checkout-model5 unsupported")
                ;; implicit: Bzr Git Hg
                ;; locking: RCS
                ;; unsupported: CVS Mtn SCCS SRC SVN
                (message
                 "vc-checkout-model5 %s" (vc-checkout-model backend tmp-name))
                (should (memq (vc-checkout-model backend tmp-name)
                              '(announce implicit locking))))))

        ;; Save exit.
        (ignore-errors
          (run-hooks 'vc-test--cleanup-hook))))))

(defun vc-test--rename-file (backend)
  "Check the rename-file action."
  (ert-with-temp-directory tempdir
    (let ((vc-handled-backends `(,backend))
          (default-directory
           (file-name-as-directory
            (expand-file-name
             (make-temp-name "vc-test") temporary-file-directory)))
          (process-environment process-environment)
          vc-test--cleanup-hook)
      (vc--fix-home-for-bzr tempdir)
      (unwind-protect
          (progn
            ;; Cleanup.
            (add-hook
             'vc-test--cleanup-hook
             (let ((dir default-directory))
               (lambda () (delete-directory dir 'recursive))))

            ;; Create empty repository.
            (make-directory default-directory)
            (vc-test--create-repo-function backend)

            (let ((tmp-name (expand-file-name "foo" default-directory))
                  (new-name (expand-file-name "bar" default-directory)))
              ;; Write a new file.
              (write-region "foo" nil tmp-name nil 'nomessage)

              ;; Register it.  Renaming can fail otherwise.
              (vc-register
               (list backend (list (file-name-nondirectory tmp-name))))

              (vc-rename-file tmp-name new-name)

              (should (not (file-exists-p tmp-name)))
              (should (file-exists-p new-name))

              (should (equal (vc-state new-name)
                             (if (memq backend '(RCS SCCS))
                                 'up-to-date
                               'added)))))

        ;; Save exit.
        (ignore-errors
          (run-hooks 'vc-test--cleanup-hook))))))

(declare-function log-edit-done "vc/log-edit")

(defun vc-test--version-diff (backend)
  "Check the diff version of a repository."
  (ert-with-temp-directory tempdir
    (let ((vc-handled-backends `(,backend))
          (default-directory
           (file-name-as-directory
            (file-truename
             (expand-file-name
              (make-temp-name "vc-test") temporary-file-directory))))
          (process-environment process-environment)
          vc-test--cleanup-hook)
      (vc--fix-home-for-bzr tempdir)
      ;; git tries various approaches to guess a user name and email,
      ;; which can fail depending on how the system is configured.
      ;; Eg if the user account has no GECOS, git commit can fail with
      ;; status 128 "fatal: empty ident name".
      (when (memq backend '(Bzr Git))
        (setq process-environment (cons "EMAIL=john@doe.ee"
                                        process-environment)))
      (if (eq backend 'Git)
          (setq process-environment (append '("GIT_AUTHOR_NAME=A"
                                              "GIT_COMMITTER_NAME=C")
                                            process-environment)))
      (unwind-protect
          (progn
            ;; Cleanup.
            (add-hook
             'vc-test--cleanup-hook
             (let ((dir default-directory))
               (lambda () (delete-directory dir 'recursive))))

            ;; Create empty repository.  Check repository checkout model.
            (make-directory default-directory)
            (vc-test--create-repo-function backend)

            (let* ((tmp-name (expand-file-name "foo" default-directory))
                   (files (list (file-name-nondirectory tmp-name))))
              ;; Write and register a new file.
              (write-region "originaltext" nil tmp-name nil 'nomessage)
              (vc-register (list backend files))

              (let ((buff (find-file tmp-name)))
                (with-current-buffer buff
                  (progn
                    ;; Optionally checkout file.
                    (when (memq backend '(RCS CVS SCCS))
                      (vc-checkout tmp-name))

                    ;; Checkin file.
                    (vc-checkin files backend)
                    (insert "Testing vc-version-diff")
                    (let (vc-async-checkin)
                      (log-edit-done)))))

              ;; Modify file content.
              (when (memq backend '(RCS CVS SCCS))
                (vc-checkout tmp-name))
              (write-region "updatedtext" nil tmp-name nil 'nomessage)

              ;; Check version diff.
              (vc-version-diff files nil nil)
              (if (eq backend 'Bzr)
                  (sleep-for 1))
              (should (bufferp (get-buffer "*vc-diff*")))

              (with-current-buffer "*vc-diff*"
                (progn
                  (let ((rawtext (buffer-substring-no-properties (point-min)
                                                                 (point-max))))
                    (should (string-search "-originaltext" rawtext))
                    (should (string-search "+updatedtext" rawtext)))))))

        ;; Save exit.
        (ignore-errors
          (run-hooks 'vc-test--cleanup-hook))))))

(defun vc-test--other-working-trees (backend)
  "Test other working trees actions."
  (ert-with-temp-directory _tempdir
    (let ((vc-handled-backends `(,backend))
          (default-directory
           (file-name-as-directory
            (expand-file-name
             (make-temp-name "vc-test") temporary-file-directory)))
          (process-environment process-environment)
          vc-test--cleanup-hook)
      (unwind-protect
          (progn
            ;; Cleanup.
            (add-hook
             'vc-test--cleanup-hook
             (let ((dir default-directory))
               (lambda ()
                 (delete-directory dir 'recursive)
                 (dolist (name '("first" "second" "first"))
                   (project-forget-project
                    (expand-file-name name default-directory))))))

            (let* ((first (file-name-as-directory
                           (expand-file-name "first" default-directory)))
                   (second (file-name-as-directory
                            (expand-file-name "second" default-directory)))
                   (third (file-name-as-directory
                           (expand-file-name "third" default-directory)))
                   (tmp-name (expand-file-name "foo" first))
                   (project-list-file
                    (expand-file-name "projects.eld" default-directory)))

              ;; Set up the first working tree.
              (make-directory first t)
              (let ((default-directory first))
                (vc-test--create-repo-function backend)
                (write-region "foo" nil tmp-name nil 'nomessage)
                (vc-register `(,backend (,(file-name-nondirectory tmp-name)))))
              (with-current-buffer (find-file-noselect tmp-name)
                (vc-checkin (list (file-name-nondirectory tmp-name)) backend)
                (insert "Testing other working trees")
                (let (vc-async-checkin)
                  (log-edit-done))

                ;; Set up the second working tree.
                ;; Stub out `vc-dir' so that it doesn't start a
                ;; background update process which won't like it when we
                ;; start moving directories around.
                ;; For the backends which do additional prompting (as
                ;; specified in the API for this backend function) we
                ;; need to stub that out.
                (cl-letf (((symbol-function 'vc-dir) #'ignore))
                  (cl-ecase backend
                    (Git (cl-letf (((symbol-function 'completing-read)
                                    (lambda (&rest _ignore) "")))
                           (vc-add-working-tree backend second)))
                    (Hg (vc-add-working-tree backend second)))))

              ;; Test `known-other-working-trees'.
              (with-current-buffer (find-file-noselect tmp-name)
                (should
                 (equal (list second)
                        (vc-call-backend backend 'known-other-working-trees)))
                (let ((default-directory second))
                  (should
                   (equal (list first)
                          (vc-call-backend backend 'known-other-working-trees))))

                ;; Test `move-working-tree'.
                (vc-move-working-tree backend second third)
                (should
                 (equal (list third)
                        (vc-call-backend backend 'known-other-working-trees)))
                (should-not (file-directory-p second))
                (should (file-directory-p third))
                ;; Moving the first working tree is only supported
                ;; for some backends.
                (cl-ecase backend
                  (Git
                   (let ((default-directory third))
                     (vc-move-working-tree backend first second))
                   (let ((default-directory third))
                     (should
                      (equal (list second)
                             (vc-call-backend backend
                                              'known-other-working-trees))))
                   (should-not (file-directory-p first))
                   (should (file-directory-p second))
                   (vc-move-working-tree backend second first))
                  (Hg
                   (let ((default-directory third))
                     (should-error (vc-move-working-tree backend
                                                         first second)))))

                ;; Test `delete-working-tree'.
                (let ((default-directory first))
                  (vc-delete-working-tree backend third)
                  (should-not (file-directory-p third))))))

        ;; Save exit.
        (ignore-errors
          (run-hooks 'vc-test--cleanup-hook))))))

;; Create the test cases.

(defun vc-test--rcs-enabled ()
  (executable-find "rcs"))

(defun vc-test--cvs-enabled ()
  (executable-find "cvs"))

(defvar vc-svn-program)
(defun vc-test--svn-enabled ()
  (and (executable-find "svnadmin")
       (executable-find vc-svn-program)))

(defun vc-test--sccs-enabled ()
  (executable-find "sccs"))

(defvar vc-src-program)
(defun vc-test--src-enabled ()
  (executable-find vc-src-program))

(defvar vc-bzr-program)
(defun vc-test--bzr-enabled ()
  (executable-find vc-bzr-program))

(defvar vc-git-program)
(defun vc-test--git-enabled ()
  (executable-find vc-git-program))

(defvar vc-hg-program)
(defun vc-test--hg-enabled ()
  (executable-find vc-hg-program))

(defvar vc-mtn-program)
(defun vc-test--mtn-enabled ()
  (executable-find vc-mtn-program))

;; Obsoleted.
(defvar vc-arch-program)
(defun vc-test--arch-enabled ()
  (executable-find vc-arch-program))

;; Create the test cases.
(dolist (backend vc-handled-backends)
  (let ((backend-string (downcase (symbol-name backend))))
    (require (intern (format "vc-%s" backend-string)))
    (eval
     ;; Check, whether the backend is supported.
     `(when (funcall ',(intern (format "vc-test--%s-enabled" backend-string)))

	(ert-deftest
	    ,(intern (format "vc-test-%s00-create-repo" backend-string)) ()
	  ,(format "Check `vc-create-repo' for the %s backend."
		   backend-string)
	  (vc-test--create-repo ',backend))

	(ert-deftest
	    ,(intern (format "vc-test-%s01-register" backend-string)) ()
	  ,(format
	    "Check `vc-register' and `vc-registered' for the %s backend."
	    backend-string)
	  (skip-unless
	   (ert-test-passed-p
	    (ert-test-most-recent-result
	     (ert-get-test
	      ',(intern
		 (format "vc-test-%s00-create-repo" backend-string))))))
	  (vc-test--register ',backend))

	(ert-deftest
	    ,(intern (format "vc-test-%s02-state" backend-string)) ()
	  ,(format "Check `vc-state' for the %s backend." backend-string)
	  (skip-unless
	   (ert-test-passed-p
	    (ert-test-most-recent-result
	     (ert-get-test
	      ',(intern
		 (format "vc-test-%s01-register" backend-string))))))
	  (vc-test--state ',backend))

	(ert-deftest
	    ,(intern (format "vc-test-%s03-working-revision" backend-string)) ()
	  ,(format "Check `vc-working-revision' for the %s backend."
		   backend-string)
	  (skip-unless
	   (ert-test-passed-p
	    (ert-test-most-recent-result
	     (ert-get-test
	      ',(intern
		 (format "vc-test-%s01-register" backend-string))))))
	  (vc-test--working-revision ',backend))

	(ert-deftest
	    ,(intern (format "vc-test-%s04-checkout-model" backend-string)) ()
	  ,(format "Check `vc-checkout-model' for the %s backend."
		   backend-string)
	  (skip-unless
	   (ert-test-passed-p
	    (ert-test-most-recent-result
	     (ert-get-test
	      ',(intern
		 (format "vc-test-%s01-register" backend-string))))))
	  (vc-test--checkout-model ',backend))

        (ert-deftest
            ,(intern (format "vc-test-%s05-rename-file" backend-string)) ()
          ,(format "Check `vc-rename-file' for the %s backend."
                   backend-string)
          (skip-unless
           (ert-test-passed-p
            (ert-test-most-recent-result
             (ert-get-test
              ',(intern
                 (format "vc-test-%s01-register" backend-string))))))
          ;; CVS calls vc-delete-file, which insists on prompting
          ;; "Really want to delete ...?", and `vc-mtn.el' does not implement
          ;; `delete-file' at all.
          (skip-when (memq ',backend '(CVS Mtn)))
          (vc-test--rename-file ',backend))

        (ert-deftest
            ,(intern (format "vc-test-%s06-version-diff" backend-string)) ()
          ,(format "Check `vc-version-diff' for the %s backend."
                   backend-string)
          (skip-unless
           (ert-test-passed-p
            (ert-test-most-recent-result
             (ert-get-test
              ',(intern
                 (format "vc-test-%s01-register" backend-string))))))
          ;; `vc-mtn.el' gives me:
          ;; "Failed (status 1): mtn commit -m Testing vc-version-diff\n\n foo"
          (skip-when (memq ',backend '(Mtn)))
          ;; `vc-hg.el' gives me, only on MS-Windows and only in batch mode:
          ;; "Failed (status 255): hg --config ui.report_untrusted=0 commit -m Testing vc-version-diff\n\n foo"
          (skip-when (and (memq ',backend '(Hg))
                          (eq system-type 'windows-nt)
                          noninteractive))
          (vc-test--version-diff ',backend))

        (ert-deftest
            ,(intern (format "vc-test-%s07-other-working-trees" backend-string)) ()
          ,(format "Check other working trees functions for the %s backend."
                   backend-string)
          (skip-unless
	   (ert-test-passed-p
	    (ert-test-most-recent-result
	     (ert-get-test
	      ',(intern
	         (format "vc-test-%s01-register" backend-string))))))
          (skip-unless (memq ',backend '(Git Hg)))
          (skip-when
           (and (eq ',backend 'Hg)
                (cl-search "failed to import extension"
                           (car
                            (process-lines-ignore-status
                             "hg" "--config=extensions.share=" "share")))))
          (let ((vc-hg-global-switches (cons "--config=extensions.share="
                                             vc-hg-global-switches)))
            (vc-test--other-working-trees ',backend)))))))

(provide 'vc-tests)
;;; vc-tests.el ends here
