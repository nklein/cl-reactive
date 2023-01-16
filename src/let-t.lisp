;;;; let-t.lisp

(in-package #:cl-reactive/tests)

(nst:def-test-group signal-let-tests ()
  (nst:def-test let-then-query (:equal nil)
    (signal-let (sig-x)
      (with-signal-values ((x sig-x))
        x)))

  (nst:def-test let-keeps-documentation (:equal "Yes")
    (signal-let ((sig-x nil :documentation "Yes"))
      (documentation sig-x t)))

  (nst:def-test let-then-set-then-query (:equal t)
    (signal-let (sig-x)
      (with-signal-values ((x sig-x))
        (setf x t)
        x)))

  (nst:def-test with-setter-enforces-type (:err :type type-error)
    (signal-let ((sig-x 0 :type integer))
      (with-signal-values ((x sig-x))
        (setf x 2/3)))))
