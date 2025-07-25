;;; data-tests.el --- tests for src/data.c  -*- lexical-binding:t -*-

;; Copyright (C) 2013-2025 Free Software Foundation, Inc.

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

;;; Code:

(require 'cl-lib)

(defconst data-tests--float-greater-than-fixnums (+ 1.0 most-positive-fixnum)
  "A floating-point value that is greater than all fixnums.
It is also as small as conveniently possible, to make the tests sharper.
Adding 1.0 to `most-positive-fixnum' should suffice on all
practical Emacs platforms, since the result is a power of 2 and
this is exactly representable and is greater than
`most-positive-fixnum', which is just less than a power of 2.")

(ert-deftest data-tests-= ()
  (should-error (=))
  (should (= 1))
  (should (= 2 2))
  (should (= 9 9 9 9 9 9 9 9 9))
  (should (= most-negative-fixnum (float most-negative-fixnum)))
  (should-not (= most-positive-fixnum data-tests--float-greater-than-fixnums))
  (should-not (apply #'= '(3 8 3)))
  (should-error (= 9 9 'foo))
  ;; Short circuits before getting to bad arg
  (should-not (= 9 8 'foo)))

(ert-deftest data-tests-< ()
  (should-error (<))
  (should (< 1))
  (should (< 2 3))
  (should (< -6 -1 0 2 3 4 8 9 999))
  (should (< 0.5 most-positive-fixnum data-tests--float-greater-than-fixnums))
  (should-not (apply #'< '(3 8 3)))
  (should-error (< 9 10 'foo))
  ;; Short circuits before getting to bad arg
  (should-not (< 9 8 'foo)))

(ert-deftest data-tests-> ()
  (should-error (>))
  (should (> 1))
  (should (> 3 2))
  (should (> 6 1 0 -2 -3 -4 -8 -9 -999))
  (should (> data-tests--float-greater-than-fixnums most-positive-fixnum 0.5))
  (should-not (apply #'> '(3 8 3)))
  (should-error (> 9 8 'foo))
  ;; Short circuits before getting to bad arg
  (should-not (> 8 9 'foo)))

(ert-deftest data-tests-<= ()
  (should-error (<=))
  (should (<= 1))
  (should (<= 2 3))
  (should (<= -6 -1 -1 0 0 0 2 3 4 8 999))
  (should (<= 0.5 most-positive-fixnum data-tests--float-greater-than-fixnums))
  (should-not (apply #'<= '(3 8 3 3)))
  (should-error (<= 9 10 'foo))
  ;; Short circuits before getting to bad arg
  (should-not (<= 9 8 'foo)))

(ert-deftest data-tests->= ()
  (should-error (>=))
  (should (>= 1))
  (should (>= 3 2))
  (should (>= 666 1 0 0 -2 -3 -3 -3 -4 -8 -8 -9 -999))
  (should (>= data-tests--float-greater-than-fixnums most-positive-fixnum))
  (should-not (apply #'>= '(3 8 3)))
  (should-error (>= 9 8 'foo))
  ;; Short circuits before getting to bad arg
  (should-not (>= 8 9 'foo)))

(ert-deftest data-tests-max ()
  (should-error (max))
  (should (= 1 (max 1)))
  (should (= 3 (max 3 2)))
  (should (= 666 (max 666 1 0 0 -2 -3 -3 -3 -4 -8 -8 -9 -999)))
  (should (= (1+ most-negative-fixnum)
             (max (float most-negative-fixnum) (1+ most-negative-fixnum))))
  (should (= 8 (apply #'max '(3 8 3))))
  (should-error (max 9 8 'foo))
  (should-error (max (make-marker)))
  (should (eql 1 (max (point-min-marker) 1))))

(ert-deftest data-tests-min ()
  (should-error (min))
  (should (= 1 (min 1)))
  (should (= 2 (min 3 2)))
  (should (= -999 (min 666 1 0 0 -2 -3 -3 -3 -4 -8 -8 -9 -999)))
  (should (= most-positive-fixnum
             (min data-tests--float-greater-than-fixnums most-positive-fixnum)))
  (should (= 3 (apply #'min '(3 8 3))))
  (should-error (min 9 8 'foo))
  (should-error (min (make-marker)))
  (should (eql 1 (min (point-min-marker) 1)))
  (should (isnan (min 0.0e+NaN)))
  (should (isnan (min 0.0e+NaN 1 2)))
  (should (isnan (min 1.0 0.0e+NaN)))
  (should (isnan (min 1.0 0.0e+NaN 1.1)))
  (should (isnan (min 1.0 0.0e+NaN 1.1 (1+ most-positive-fixnum))))
  (should (isnan (max 1.0 0.0e+NaN 1.1 (1+ most-positive-fixnum)))))

(defun data-tests-popcnt (byte)
  "Calculate the Hamming weight of BYTE."
  (if (< byte 0)
      (setq byte (lognot byte)))
  (if (zerop byte)
      0
    (+ (logand byte 1) (data-tests-popcnt (ash byte -1)))))

(ert-deftest data-tests-logcount ()
  (should (cl-loop for n in (number-sequence -255 255)
                   always (= (logcount n) (data-tests-popcnt n))))
  ;; https://oeis.org/A000120
  (should (= 11 (logcount 9727)))
  (should (= 8 (logcount 9999))))

;; Bool vector tests.  Compactly represent bool vectors as hex
;; strings.

(ert-deftest bool-vector-count-population-all-0-nil ()
  (cl-loop for sz in '(0 45 1 64 9 344)
           do (let* ((bv (make-bool-vector sz nil)))
                (should
                 (zerop
                  (bool-vector-count-population bv))))))

(ert-deftest bool-vector-count-population-all-1-t ()
  (cl-loop for sz in '(0 45 1 64 9 344)
           do (let* ((bv (make-bool-vector sz t)))
                (should
                 (eql
                  (bool-vector-count-population bv)
                  sz)))))

(ert-deftest bool-vector-count-population-1-nil ()
  (let* ((bv (make-bool-vector 45 nil)))
    (aset bv 40 t)
    (aset bv 0 t)
    (should
     (eql
      (bool-vector-count-population bv)
      2))))

(ert-deftest bool-vector-count-population-1-t ()
  (let* ((bv (make-bool-vector 45 t)))
    (aset bv 40 nil)
    (aset bv 0 nil)
    (should
     (eql
      (bool-vector-count-population bv)
      43))))

(defun mock-bool-vector-count-consecutive (a b i)
  (cl-loop for i from i below (length a)
           while (eq (aref a i) b)
           sum 1))

(defun test-bool-vector-bv-from-hex-string (desc)
  (let (bv nibbles)
    (dolist (c (string-to-list desc))
      (push (string-to-number
             (char-to-string c)
             16)
            nibbles))
    (setf bv (make-bool-vector (* 4 (length nibbles)) nil))
    (let ((i 0))
      (dolist (n (nreverse nibbles))
        (dotimes (_ 4)
          (aset bv i (oddp n))
          (incf i)
          (setf n (ash n -1)))))
    bv))

(defun test-bool-vector-to-hex-string (bv)
  (let (nibbles (v (cl-coerce bv 'list)))
    (while v
      (push (logior
             (ash (if (nth 0 v) 1 0) 0)
             (ash (if (nth 1 v) 1 0) 1)
             (ash (if (nth 2 v) 1 0) 2)
             (ash (if (nth 3 v) 1 0) 3))
            nibbles)
      (setf v (nthcdr 4 v)))
    (mapconcat (lambda (n) (format "%X" n))
               (nreverse nibbles))))

(defun test-bool-vector-count-consecutive-tc (desc)
  "Run a test case for `bool-vector-count-consecutive'.
DESC is a string describing the test.  It is a sequence of
hexadecimal digits describing the bool vector.  We exhaustively
test all counts at all possible positions in the vector by
comparing the subr with a much slower Lisp implementation."
  (let ((bv (test-bool-vector-bv-from-hex-string desc)))
    (cl-loop
     for lf in '(nil t)
     do (cl-loop
         for pos from 0 upto (length bv)
         for cnt = (mock-bool-vector-count-consecutive bv lf pos)
         for rcnt = (bool-vector-count-consecutive bv lf pos)
         unless (eql cnt rcnt)
         do (error "FAILED testcase %S %3S %3S %3S"
                   pos lf cnt rcnt)))))

(ert-deftest binding-test-makunbound-built-in ()
  "Verify that attempts to `makunbound' built-in symbols are rejected."
  (should-error (makunbound 'initial-window-system))
  (let ((initial-window-system 'x))
    (should-error (makunbound 'initial-window-system)))
  (should-error
   (makunbound (make-local-variable 'initial-window-system)))
  (let ((initial-window-system 'x))
    (should-error (makunbound 'initial-window-system))))

(defconst bool-vector-test-vectors
'(""
  "0"
  "F"
  "0F"
  "F0"
  "00000000000000000000000000000FFFFF0000000"
  "44a50234053fba3340000023444a50234053fba33400000234"
  "12341234123456123412346001234123412345612341234600"
  "44a50234053fba33400000234"
  "1234123412345612341234600"
  "44a50234053fba33400000234"
  "1234123412345612341234600"
  "44a502340"
  "123412341"
  "0000000000000000000000000"
  "FFFFFFFFFFFFFFFF1"))

(ert-deftest bool-vector-count-consecutive ()
  (mapc #'test-bool-vector-count-consecutive-tc
        bool-vector-test-vectors))

(defun test-bool-vector-apply-mock-op (mock a b c)
  "Compute (slowly) the correct result of a bool-vector set operation."
  (let (changed)
    (cl-assert (eql (length b) (length c)))
    (unless a
      (setf a (make-bool-vector (length b) nil))
      (setf changed t))

    (cl-loop for i below (length b)
             for mockr = (funcall mock
                                  (if (aref b i) 1 0)
                                  (if (aref c i) 1 0))
             for r = (not (= 0 mockr))
             do (progn
                  (unless (eq (aref a i) r)
                    (setf changed t))
                  (setf (aref a i) r)))
    (if changed a)))

(defun test-bool-vector-binop (mock real)
  "Test a binary set operation."
  (cl-loop for s1 in bool-vector-test-vectors
           for bv1 = (test-bool-vector-bv-from-hex-string s1)
           for vecs2 = (cl-remove-if-not
                        (lambda (x) (eql (length x) (length s1)))
                        bool-vector-test-vectors)
           do (cl-loop for s2 in vecs2
                       for bv2 = (test-bool-vector-bv-from-hex-string s2)
                       for mock-result = (test-bool-vector-apply-mock-op
                                          mock nil bv1 bv2)
                       for real-result = (funcall real bv1 bv2)
                       do (progn
                            (should (equal mock-result real-result))))))

(ert-deftest bool-vector-intersection-op ()
  (test-bool-vector-binop
   #'logand
   #'bool-vector-intersection))

(ert-deftest bool-vector-union-op ()
  (test-bool-vector-binop
   #'logior
   #'bool-vector-union))

(ert-deftest bool-vector-xor-op ()
  (test-bool-vector-binop
   #'logxor
   #'bool-vector-exclusive-or))

(ert-deftest bool-vector-set-difference-op ()
  (test-bool-vector-binop
   (lambda (a b) (logand a (lognot b)))
   #'bool-vector-set-difference))

(ert-deftest bool-vector-change-detection ()
  (let* ((vc1 (test-bool-vector-bv-from-hex-string "abcdef"))
         (vc2 (test-bool-vector-bv-from-hex-string "012345"))
         (vc3 (make-bool-vector (length vc1) nil))
         (c1 (bool-vector-union vc1 vc2 vc3))
         (c2 (bool-vector-union vc1 vc2 vc3)))
    (should (equal c1 (test-bool-vector-apply-mock-op
                       #'logior
                       nil
                       vc1 vc2)))
    (should (not c2))))

(ert-deftest bool-vector-not ()
  (let* ((v1 (test-bool-vector-bv-from-hex-string "FFFF3"))
         (v2 (test-bool-vector-bv-from-hex-string "0000C"))
         (v3 (bool-vector-not v1)))
    (should (equal v2 v3))))

;; Tests for variable bindings

(defvar binding-test-buffer-A (get-buffer-create "A"))
(defvar binding-test-buffer-B (get-buffer-create "B"))

(defvar binding-test-always-local 'always)
(make-variable-buffer-local 'binding-test-always-local)

(defvar binding-test-some-local 'some)
(with-current-buffer binding-test-buffer-A
  (setq-local binding-test-some-local 'local))

(ert-deftest binding-test-manual ()
  "A test case from the elisp manual."
  (with-current-buffer binding-test-buffer-A
    (let ((binding-test-some-local 'something-else))
      (should (eq binding-test-some-local 'something-else))
      (set-buffer binding-test-buffer-B)
      (should (eq binding-test-some-local 'some)))
    (should (eq binding-test-some-local 'some))
    (set-buffer binding-test-buffer-A)
    (should (eq binding-test-some-local 'local))))

(ert-deftest binding-test-setq-default ()
  "Test that a `setq-default' has no effect when there is a local binding."
  (with-current-buffer binding-test-buffer-B
    ;; This variable is not local in this buffer.
    (let ((binding-test-some-local 'something-else))
      (setq-default binding-test-some-local 'new-default))
    (should (eq binding-test-some-local 'some))))

(defvar c-e-x)
(ert-deftest binding-test-defvar-in-let ()
  "Test some core Elisp rules."
  (with-temp-buffer
    ;; Check that when defvar is run within a let-binding, the toplevel default
    ;; is properly initialized.
    (should (equal (list (let ((c-e-x 1)) (defvar c-e-x 2) c-e-x) c-e-x)
                   '(1 2)))
    (should (equal (list (let ((c-e-x 1))
                           (defcustom c-e-x 2 "doc" :group 'blah :type 'integer) c-e-x)
                         c-e-x)
                   '(1 2)))))

(ert-deftest data-tests--let-buffer-local ()
  (let ((blvar (make-symbol "blvar")))
    (set-default blvar nil)
    (make-variable-buffer-local blvar)

    (dolist (var (list blvar 'left-margin))
      (let ((def (default-value var)))
        (with-temp-buffer
          (should (equal def (symbol-value var)))
          (cl-progv (list var) (list 42)
            (should (equal (symbol-value var) 42))
            (should (equal (default-value var) (symbol-value var)))
            (set var 123)
            (should (not (local-variable-p var)))
            (should (equal (symbol-value var) 123))
            (should (equal (default-value var) (symbol-value var)))) ;bug#44733
          (should (equal (symbol-value var) def))
          (should (equal (default-value var) (symbol-value var))))
        (should (equal (default-value var) def))))))

(ert-deftest data-tests--let-buffer-local-no-unwind-other-buffers ()
  "Test that a let-binding for a buffer-local unwinds only current-buffer."
  (let ((blvar (make-symbol "blvar")))
    (set-default blvar 0)
    (make-variable-buffer-local blvar)
    (dolist (var (list blvar 'left-margin))
      (let* ((def (default-value var))
             (newdef (+ def 1))
             (otherbuf (generate-new-buffer "otherbuf")))
        (with-temp-buffer
          (cl-progv (list var) (list newdef)
            (with-current-buffer otherbuf
              (set var 123)
              (should (local-variable-p var))
              (should (equal (symbol-value var) 123))
              (should (equal (default-value var) newdef))))
          (with-current-buffer otherbuf
            (should (local-variable-p var))
            (should (equal (symbol-value var) 123))
            (should (equal (default-value var) def)))
          )))))

(defvar-local c-e-l 'foo)
(ert-deftest binding-test-toplevel-values ()
  (setq-default c-e-l 'foo)
  (let ((c-e-l 'bar))
    (let ((c-e-l 'baz))
      (setq-default c-e-l 'bar)
      (should (eq c-e-l 'bar))
      (should (eq (default-toplevel-value 'c-e-l) 'foo))
      (set-default-toplevel-value 'c-e-l 'baz)
      (should (eq c-e-l 'bar))
      (should (eq (default-toplevel-value 'c-e-l) 'baz))))
  (let ((c-e-u 'foo))
    (ignore c-e-u)   ; hush unused-variable warning
    (should (condition-case _
                (default-toplevel-value 'c-e-u)
              (void-variable t))))
  (with-temp-buffer
    (setq-local c-e-l 'bar)
    (should (eq (buffer-local-toplevel-value 'c-e-l) 'bar))
    (let ((c-e-l 'baz))
      (let ((c-e-l 'quux))
        (setq-local c-e-l 'baz)
        (should (eq c-e-l 'baz))
        (should (eq (buffer-local-toplevel-value 'c-e-l) 'bar))
        (set-buffer-local-toplevel-value 'c-e-l 'foo)
        (should (eq c-e-l 'baz))
        (should (eq (buffer-local-toplevel-value 'c-e-l) 'foo)))))
  (with-temp-buffer
    (should (condition-case _
                (buffer-local-toplevel-value 'c-e-l)
              (void-variable t)))))

(ert-deftest binding-test-makunbound ()
  "Tests of makunbound, from the manual."
  (with-current-buffer binding-test-buffer-B
    (should (boundp 'binding-test-some-local))
    (let ((binding-test-some-local 'outer))
      (let ((binding-test-some-local 'inner))
	(makunbound 'binding-test-some-local)
	(should (not (boundp 'binding-test-some-local))))
      (should (and (boundp 'binding-test-some-local)
		   (eq binding-test-some-local 'outer))))))

(ert-deftest binding-test-defvar-bool ()
  "Test DEFVAR_BOOL."
  (let ((display-hourglass 5))
    (should (eq display-hourglass t))))

(ert-deftest binding-test-defvar-int ()
  "Test DEFVAR_INT."
  (should-error (setq gc-cons-threshold 5.0) :type 'wrong-type-argument))

(ert-deftest binding-test-set-constant-t ()
  "Test setting the constant t."
  (with-no-warnings (should-error (setq t 'bob) :type 'setting-constant)))

(ert-deftest binding-test-set-constant-nil ()
  "Test setting the constant nil."
  (with-no-warnings (should-error (setq nil 'bob) :type 'setting-constant)))

(ert-deftest binding-test-set-constant-keyword ()
  "Test setting a keyword constant."
  (with-no-warnings (should-error (setq :keyword 'bob) :type 'setting-constant)))

(ert-deftest binding-test-set-constant-itself ()
  "Test setting a keyword to itself."
  (with-no-warnings (should (setq :keyword :keyword))))

(ert-deftest data-tests--set-default-per-buffer ()
  :expected-result t ;; Not fixed yet!
  ;; FIXME: Performance tests are inherently unreliable.
  ;; Using wall-clock time makes it even worse, so don't bother unless
  ;; we have the primitive to measure cpu-time.
  (skip-unless (fboundp 'current-cpu-time))
  ;; Test performance of set-default on DEFVAR_PER_BUFFER variables.
  ;; More specifically, test the problem seen in bug#41029 where setting
  ;; the default value of a variable takes time proportional to the
  ;; number of buffers.
  (when (fboundp 'current-cpu-time)     ; silence byte-compiler
    (let* ((fun #'error)
           (test (lambda ()
                   (with-temp-buffer
                     (let ((st (car (current-cpu-time))))
                       (dotimes (_ 1000)
                         (let ((case-fold-search 'data-test))
                           ;; Use an indirection through a mutable var
                           ;; to try and make sure the byte-compiler
                           ;; doesn't optimize away the let bindings.
                           (funcall fun)))
                       ;; FIXME: Handle the wraparound, if any.
                       (- (car (current-cpu-time)) st)))))
           (_ (setq fun #'ignore))
           (time1 (funcall test))
           (bufs (mapcar (lambda (_) (generate-new-buffer " data-test"))
                         (make-list 1000 nil)))
           (time2 (funcall test)))
      (mapc #'kill-buffer bufs)
      ;; Don't divide one time by the other since they may be 0.
      (should (< time2 (* time1 5))))))

;; More tests to write -
;; kill-local-variable
;; defconst; can modify
;; defvar and defconst modify the local binding [ doesn't matter for us ]
;; various kinds of special internal forwarding objects
;;   a couple examples in manual, not enough
;; variable aliases

;; Tests for watchpoints

(ert-deftest data-tests-variable-watchers ()
  (defvar data-tests-var 0)
  (let* ((watch-data nil)
         (collect-watch-data
          (lambda (&rest args) (push args watch-data))))
    (cl-flet ((should-have-watch-data (data)
                                      (should (equal (pop watch-data) data))
                                      (should (null watch-data))))
      (add-variable-watcher 'data-tests-var collect-watch-data)
      (setq data-tests-var 1)
      (should-have-watch-data '(data-tests-var 1 set nil))
      (let ((data-tests-var 2))
        (should-have-watch-data '(data-tests-var 2 let nil))
        (setq data-tests-var 3)
        (should-have-watch-data '(data-tests-var 3 set nil)))
      (should-have-watch-data '(data-tests-var 1 unlet nil))
      ;; `setq-default' on non-local variable is same as `setq'.
      (setq-default data-tests-var 4)
      (should-have-watch-data '(data-tests-var 4 set nil))
      (makunbound 'data-tests-var)
      (should-have-watch-data '(data-tests-var nil makunbound nil))
      (setq data-tests-var 5)
      (should-have-watch-data '(data-tests-var 5 set nil))
      (remove-variable-watcher 'data-tests-var collect-watch-data)
      (setq data-tests-var 6)
      (should (null watch-data)))))

(ert-deftest data-tests-varalias-watchers ()
  (defvar data-tests-var0 0)
  (defvar data-tests-var1 0)
  (defvar data-tests-var2 0)
  (defvar data-tests-var3 0)
  (let* ((watch-data nil)
         (collect-watch-data
          (lambda (&rest args) (push args watch-data))))
    (cl-flet ((should-have-watch-data (data)
                                      (should (equal (pop watch-data) data))
                                      (should (null watch-data))))
      ;; Watch var0, then alias it.
      (add-variable-watcher 'data-tests-var0 collect-watch-data)
      (defvar data-tests-var0-alias)
      (defvaralias 'data-tests-var0-alias 'data-tests-var0)
      (setq data-tests-var0 1)
      (should-have-watch-data '(data-tests-var0 1 set nil))
      (setq data-tests-var0-alias 2)
      (should-have-watch-data '(data-tests-var0 2 set nil))
      ;; Alias var1, then watch var1-alias.
      (defvar data-tests-var1-alias)
      (defvaralias 'data-tests-var1-alias 'data-tests-var1)
      (add-variable-watcher 'data-tests-var1-alias collect-watch-data)
      (setq data-tests-var1 1)
      (should-have-watch-data '(data-tests-var1 1 set nil))
      (setq data-tests-var1-alias 2)
      (should-have-watch-data '(data-tests-var1 2 set nil))
      ;; Alias var2, then watch it.
      (defvar data-tests-var2-alias)
      (defvaralias 'data-tests-var2-alias 'data-tests-var2)
      (add-variable-watcher 'data-tests-var2 collect-watch-data)
      (setq data-tests-var2 1)
      (should-have-watch-data '(data-tests-var2 1 set nil))
      (setq data-tests-var2-alias 2)
      (should-have-watch-data '(data-tests-var2 2 set nil))
      ;; Watch var3-alias, then make it alias var3 (this removes the
      ;; watcher flag).
      (defvar data-tests-var3-alias 0)
      (add-variable-watcher 'data-tests-var3-alias collect-watch-data)
      (defvaralias 'data-tests-var3-alias 'data-tests-var3)
      (should-have-watch-data '(data-tests-var3-alias
                                data-tests-var3 defvaralias nil))
      (setq data-tests-var3 1)
      (setq data-tests-var3-alias 2)
      (should (null watch-data)))))

(ert-deftest data-tests-local-variable-watchers ()
  (with-no-warnings
    (defvar-local data-tests-lvar 0))
  (let* ((buf1 (current-buffer))
         (buf2 nil)
         (watch-data nil)
         (collect-watch-data
          (lambda (&rest args) (push args watch-data))))
    (cl-flet ((should-have-watch-data (data)
                                      (should (equal (pop watch-data) data))
                                      (should (null watch-data))))
      (add-variable-watcher 'data-tests-lvar collect-watch-data)
      (setq data-tests-lvar 1)
      (should-have-watch-data `(data-tests-lvar 1 set ,buf1))
      (let ((data-tests-lvar 2))
        (should-have-watch-data `(data-tests-lvar 2 let ,buf1))
        (setq data-tests-lvar 3)
        (should-have-watch-data `(data-tests-lvar 3 set ,buf1)))
      (should-have-watch-data `(data-tests-lvar 1 unlet ,buf1))
      (setq-default data-tests-lvar 4)
      (should-have-watch-data '(data-tests-lvar 4 set nil))
      (with-temp-buffer
        (setq buf2 (current-buffer))
        (setq data-tests-lvar 1)
        (should-have-watch-data `(data-tests-lvar 1 set ,buf2))
        (let ((data-tests-lvar 2))
          (should-have-watch-data `(data-tests-lvar 2 let ,buf2))
          (setq data-tests-lvar 3)
          (should-have-watch-data `(data-tests-lvar 3 set ,buf2)))
        (should-have-watch-data `(data-tests-lvar 1 unlet ,buf2))
        (kill-local-variable 'data-tests-lvar)
        (should-have-watch-data `(data-tests-lvar nil makunbound ,buf2))
        (setq data-tests-lvar 3.5)
        (should-have-watch-data `(data-tests-lvar 3.5 set ,buf2))
        (kill-all-local-variables)
        (should-have-watch-data `(data-tests-lvar nil makunbound ,buf2)))
      (setq-default data-tests-lvar 4)
      (should-have-watch-data '(data-tests-lvar 4 set nil))
      (makunbound 'data-tests-lvar)
      (should-have-watch-data '(data-tests-lvar nil makunbound nil))
      (setq data-tests-lvar 5)
      (should-have-watch-data `(data-tests-lvar 5 set ,buf1))
      (remove-variable-watcher 'data-tests-lvar collect-watch-data)
      (setq data-tests-lvar 6)
      (should (null watch-data)))))

(ert-deftest data-tests-kill-all-local-variables () ;bug#30846
  (with-temp-buffer
    (setq-local data-tests-foo1 1)
    (setq-local data-tests-foo2 2)
    (setq-local data-tests-foo3 3)
    (let ((oldfoo2 nil))
      (add-variable-watcher 'data-tests-foo2
                            (lambda (&rest _)
                              (setq oldfoo2 (bound-and-true-p data-tests-foo2))))
      (kill-all-local-variables)
      (should (equal oldfoo2 '2)) ;Watcher is run before changing the var.
      (should (not (or (bound-and-true-p data-tests-foo1)
                       (bound-and-true-p data-tests-foo2)
                       (bound-and-true-p data-tests-foo3)))))))

(ert-deftest data-tests-bignum ()
  (should (bignump (+ most-positive-fixnum 1)))
  (let ((f0 (+ (float most-positive-fixnum) 1))
        (f-1 (- (float most-negative-fixnum) 1))
        (b0 (+ most-positive-fixnum 1))
        (b-1 (- most-negative-fixnum 1)))
    (should (> b0 -1))
    (should (> b0 f-1))
    (should (> b0 b-1))
    (should (>= b0 -1))
    (should (>= b0 f-1))
    (should (>= b0 b-1))
    (should (>= b-1 b-1))

    (should (< -1 b0))
    (should (< f-1 b0))
    (should (< b-1 b0))
    (should (<= -1 b0))
    (should (<= f-1 b0))
    (should (<= b-1 b0))
    (should (<= b-1 b-1))

    (should (= (+ f0 b0) (+ b0 f0)))
    (should (= (+ f0 b-1) (+ b-1 f0)))
    (should (= (+ f-1 b0) (+ b0 f-1)))
    (should (= (+ f-1 b-1) (+ b-1 f-1)))

    (should (= (* f0 b0) (* b0 f0)))
    (should (= (* f0 b-1) (* b-1 f0)))
    (should (= (* f-1 b0) (* b0 f-1)))
    (should (= (* f-1 b-1) (* b-1 f-1)))

    (should (= b0 f0))
    (should (= b0 b0))

    (should (/= b0 f-1))
    (should (/= b0 b-1))

    (should (/= b0 0.0e+NaN))
    (should (/= b-1 0.0e+NaN))))

(ert-deftest data-tests-+ ()
  (should-not (fixnump (+ most-positive-fixnum most-positive-fixnum)))
  (should (> (+ most-positive-fixnum most-positive-fixnum) most-positive-fixnum))
  (should (eq (- (+ most-positive-fixnum most-positive-fixnum)
                 (+ most-positive-fixnum most-positive-fixnum))
              0)))

(ert-deftest data-tests-/ ()
  (let* ((x (* most-positive-fixnum 8))
         (y (* most-negative-fixnum 8))
         (z (- y)))
    (should (= most-positive-fixnum (/ x 8)))
    (should (= most-negative-fixnum (/ y 8)))
    (should (= -1 (/ y z)))
    (should (= -1 (/ z y)))
    (should (= 0 (/ x (* 2 x))))
    (should (= 0 (/ y (* 2 y))))
    (should (= 0 (/ z (* 2 z))))))

(ert-deftest data-tests-number-predicates ()
  (should (fixnump 0))
  (should (fixnump most-negative-fixnum))
  (should (fixnump most-positive-fixnum))
  (should (integerp (+ most-positive-fixnum 1)))
  (should (integer-or-marker-p (+ most-positive-fixnum 1)))
  (should (numberp (+ most-positive-fixnum 1)))
  (should (number-or-marker-p (+ most-positive-fixnum 1)))
  (should (natnump (+ most-positive-fixnum 1)))
  (should-not (fixnump (+ most-positive-fixnum 1)))
  (should (bignump (+ most-positive-fixnum 1))))

(ert-deftest data-tests-number-to-string ()
  (let* ((s "99999999999999999999999999999")
         (v (read s)))
    (should (equal (number-to-string v) s))))

(ert-deftest data-tests-1+ ()
  (should (> (1+ most-positive-fixnum) most-positive-fixnum))
  (should (fixnump (1+ (1- most-negative-fixnum)))))

(ert-deftest data-tests-1- ()
  (should (< (1- most-negative-fixnum) most-negative-fixnum))
  (should (fixnump (1- (1+ most-positive-fixnum)))))

(ert-deftest data-tests-logand ()
  (should (= -1 (logand) (logand -1) (logand -1 -1)))
  (let ((n (1+ most-positive-fixnum)))
    (should (= (logand -1 n) n)))
  (let ((n (* 2 most-negative-fixnum)))
    (should (= (logand -1 n) n))))

(ert-deftest data-tests-logcount-2 ()
  (should (= (logcount (read "#xffffffffffffffffffffffffffffffff")) 128)))

(ert-deftest data-tests-logior ()
  (should (= -1 (logior -1) (logior -1 -1)))
  (should (= -1 (logior most-positive-fixnum most-negative-fixnum))))

(ert-deftest data-tests-logxor ()
  (should (= -1 (logxor -1) (logxor -1 -1 -1)))
  (let ((n (1+ most-positive-fixnum)))
    (should (= (logxor -1 n) (lognot n)))))

(ert-deftest data-tests-minmax ()
  (let ((a (- most-negative-fixnum 1))
        (b (+ most-positive-fixnum 1))
        (c 0))
    (should (= (min a b c) a))
    (should (= (max a b c) b))))

(defun data-tests-check-sign (x y)
  (should (eq (cl-signum x) (cl-signum y))))

(ert-deftest data-tests-%-mod ()
  (let* ((b1 (+ most-positive-fixnum 1))
         (nb1 (- b1))
         (b3 (+ most-positive-fixnum 3))
         (nb3 (- b3)))
    (data-tests-check-sign (% 1 3) (% b1 b3))
    (data-tests-check-sign (mod 1 3) (mod b1 b3))
    (data-tests-check-sign (% 1 -3) (% b1 nb3))
    (data-tests-check-sign (mod 1 -3) (mod b1 nb3))
    (data-tests-check-sign (% -1 3) (% nb1 b3))
    (data-tests-check-sign (mod -1 3) (mod nb1 b3))
    (data-tests-check-sign (% -1 -3) (% nb1 nb3))
    (data-tests-check-sign (mod -1 -3) (mod nb1 nb3))))

(ert-deftest data-tests-mod-0 ()
  (dolist (num (list (1- most-negative-fixnum) -1 0 1
                     (1+ most-positive-fixnum)))
    (should-error (mod num 0)))
  (when (ignore-errors (/ 0.0 0))
    (should (equal (abs (mod 0.0 0)) (abs (- 0.0 (/ 0.0 0)))))))

(ert-deftest data-tests-ash-lsh ()
  (should (= (ash most-negative-fixnum 1)
             (* most-negative-fixnum 2)))
  (should (= (ash 0 (* 2 most-positive-fixnum)) 0))
  (should (= (ash 1000 (* 2 most-negative-fixnum)) 0))
  (should (= (ash -1000 (* 2 most-negative-fixnum)) -1))
  (should (= (ash (* 2 most-negative-fixnum) (* 2 most-negative-fixnum)) -1))
  (should (= (ash (* 2 most-negative-fixnum) -1)
	     most-negative-fixnum))
  (with-suppressed-warnings ((suspicious lsh))
    (should (= (lsh most-negative-fixnum 1)
               (* most-negative-fixnum 2)))
    (should (= (lsh most-positive-fixnum -1) (/ most-positive-fixnum 2)))
    (should (= (lsh most-negative-fixnum -1) (lsh (- most-negative-fixnum) -1)))
    (should (= (lsh -1 -1) most-positive-fixnum))
    (should-error (lsh (1- most-negative-fixnum) -1))))

(ert-deftest data-tests-make-local-forwarded-var () ;bug#34318
  ;; Boy, this bug is tricky to trigger.  You need to:
  ;; - call make-local-variable on a forwarded var (i.e. one that
  ;;   has a corresponding C var linked via DEFVAR_(LISP|INT|BOOL))
  ;; - cause the C code to modify this variable from the C side of the
  ;;   forwarding, but this needs to happen before the var is accessed
  ;;   from the Lisp side and before we switch to another buffer.
  ;; The trigger in bug#34318 doesn't exist any more because the C code has
  ;; changed.  Instead I found the trigger below.
  (with-temp-buffer
    (setq last-coding-system-used 'bug34318)
    (make-local-variable 'last-coding-system-used)
    ;; This should set last-coding-system-used to `no-conversion'.
    (decode-coding-string "hello" nil)
    (should (equal (list last-coding-system-used
                         (default-value 'last-coding-system-used))
                   '(no-conversion bug34318)))))

(defvar-local data-tests--bug65209 :default-value)

(ert-deftest data-tests-make-local-bug65209 ()
  (dolist (sym '(data-tests--bug65209   ;A normal always-local Lisp var.
                 cursor-in-non-selected-windows)) ;Same but DEFVAR_PER_BUFFER.
    ;; Note: For vars like `mode-name' that are *really* always buffer-local,
    ;; this test isn't right because the `cl-progv' only binds the
    ;; buffer-local value!
    (let ((default (default-value sym))
          vli vlo vgi vgo)
      (with-temp-buffer
        (cl-progv (list sym) '(:let-bound-value)
          ;; While `setq' would not make the var buffer-local
          ;; (because we'd be setq-ing the let-binding instead),
          ;; `setq-local' definitely should.
          (set (make-local-variable sym) :buffer-local-value)
          (setq vgi (with-temp-buffer (symbol-value sym)))
          (setq vli (symbol-value sym)))
      (setq vgo (with-temp-buffer (symbol-value sym)))
      (setq vlo (symbol-value sym)))
      (should (equal (list vgo vgi vlo vli)
                     (cons default
                           '(:let-bound-value
                             :buffer-local-value :buffer-local-value)))))))

(ert-deftest data-tests-make_symbol_constant ()
  "Can't set variable marked with 'make_symbol_constant'."
  (should-error (setq most-positive-fixnum 1) :type 'setting-constant))

(ert-deftest data-tests-fset ()
  (fset 'data-tests--fs-fun (lambda () 'moo))
  (declare-function data-tests--fs-fun nil)
  (should (equal (data-tests--fs-fun) 'moo))

  (fset 'data-tests--fs-fun1 'data-tests--fs-fun)
  (declare-function data-tests--fs-fun1 nil)
  (should (equal (data-tests--fs-fun1) 'moo))

  (fset 'data-tests--fs-a 'data-tests--fs-b)
  (fset 'data-tests--fs-b 'data-tests--fs-c)

  (should-error (fset 'data-tests--fs-c 'data-tests--fs-c)
                :type 'cyclic-function-indirection)
  (fset 'data-tests--fs-d 'data-tests--fs-a)
  (should-error (fset 'data-tests--fs-c 'data-tests--fs-d)
                :type 'cyclic-function-indirection))

(ert-deftest data-tests-defalias ()
  (defalias 'data-tests--da-fun (lambda () 'baa))
  (declare-function data-tests--da-fun nil)
  (should (equal (data-tests--da-fun) 'baa))

  (defalias 'data-tests--da-fun1 'data-tests--da-fun)
  (declare-function data-tests--da-fun1 nil)
  (should (equal (data-tests--da-fun1) 'baa))

  (defalias 'data-tests--da-a 'data-tests--da-b)
  (defalias 'data-tests--da-b 'data-tests--da-c)

  (should-error (defalias 'data-tests--da-c 'data-tests--da-c)
                :type 'cyclic-function-indirection)
  (defalias 'data-tests--da-d 'data-tests--da-a)
  (should-error (defalias 'data-tests--da-c 'data-tests--da-d)
                :type 'cyclic-function-indirection))

(ert-deftest data-tests-bare-symbol ()
  (dolist (symbols-with-pos-enabled '(nil t))
    (dolist (sym (list nil t 'xyzzy (make-symbol "")))
      (should (eq sym (bare-symbol (position-symbol sym 0)))))))

(require 'cl-extra)                     ;For `cl--class-children'.

(ert-deftest data-tests--cl-type-of ()
  ;; Make sure that `cl-type-of' returns the most precise type.
  ;; Note: This doesn't work for list/vector structs since those types
  ;; are too difficult/unreliable to detect (so `cl-type-of' only says
  ;; it's a `cons' or a `vector').
  (dolist (val (list -2 10 (expt 2 128) nil t 'car :car
                     (symbol-function 'car)
                     (symbol-function 'progn)
                     (eval '(lambda (x) (+ x 1)) t)
                     (position-symbol 'car 7)
                     (position-symbol :car 7)))
    (let* ((type (cl-type-of val))
           (class (cl-find-class type))
           (alltypes (cl--class-allparents class))
           ;; FIXME: Our type DAG is affected by `symbols-with-pos-enabled'.
           ;; (e.g. `symbolp' returns nil on a sympos if that var is nil).
           (symbols-with-pos-enabled t))
      (dolist (parent alltypes)
        (should (cl-typep val parent))
        (dolist (subtype (cl--class-children (cl-find-class parent)))
          (when (and (not (memq subtype alltypes))
                     (built-in-class-p (cl-find-class subtype))
                     (not (memq subtype
                                ;; FIXME: Some types don't have any associated
                                ;; predicate,
                                '( font-spec font-entity font-object
                                   finalizer condvar terminal
                                   native-comp-unit interpreted-function
                                   tree-sitter-compiled-query
                                   tree-sitter-node tree-sitter-parser))))
            (cond
             ((eq subtype 'function) (cl-functionp val))
             (t (should-not (cl-typep val subtype))))))))))

;;; data-tests.el ends here
