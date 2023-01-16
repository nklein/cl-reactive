;;;; package-t.lisp

(defpackage #:cl-reactive/tests
  (:use #:cl-reactive #:cl)
  (:export #:run-all-tests))

(in-package #:cl-reactive/tests)

(defun run-all-tests (&optional (*print-pretty* t))
  (nst:nst-cmd :run-package #.*package*))
