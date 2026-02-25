;;; charset-tests.el --- Tests for charset.c -*- lexical-binding: t -*-

;; Copyright 2017-2026 Free Software Foundation, Inc.

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

;;; Code:

(require 'ert)

(ert-deftest charset-decode-char ()
  "Test `decode-char'."
  (should-error (decode-char 'ascii 0.5)))

(ert-deftest charset-tests-charsetp ()
  (should (charsetp 'ascii))
  (should (charsetp 'unicode))
  (should-not (charsetp 'charset-tests-no-such-charset)))

(ert-deftest charset-tests-charset-id-internal ()
  (let ((id (charset-id-internal 'ascii)))
    (should (integerp id))
    (should (<= 0 id))))

(ert-deftest charset-tests-charset-plist ()
  (let ((plist (charset-plist 'ascii)))
    (should (listp plist))
    (should (stringp (plist-get plist :short-name)))))

(ert-deftest charset-tests-charset-priority-list ()
  (let ((list (charset-priority-list)))
    (should (listp list))
    (should (consp list))
    (should (memq 'ascii list))
    (dolist (cs list)
      (should (charsetp cs))))
  (let ((highest (charset-priority-list t)))
    (should (symbolp highest))
    (should (charsetp highest))))

(ert-deftest charset-tests-charset-after ()
  (with-temp-buffer
    (insert "a")
    (goto-char (point-min))
    (should (eq (charset-after) 'ascii))
    (should-not (charset-after (1+ (point-max))))))

(ert-deftest charset-tests-find-charset-string ()
  (let ((charsets (find-charset-string "abc")))
    (should (memq 'ascii charsets))
    (dolist (cs charsets)
      (should (charsetp cs))))
  (let ((charsets (find-charset-string "あ")))
    (should (consp charsets))
    (dolist (cs charsets)
      (should (charsetp cs)))))

(ert-deftest charset-tests-find-charset-region ()
  (with-temp-buffer
    (insert "abc")
    (let ((charsets (find-charset-region (point-min) (point-max))))
      (should (memq 'ascii charsets))
      (dolist (cs charsets)
        (should (charsetp cs))))))

(provide 'charset-tests)

;;; charset-tests.el ends here
