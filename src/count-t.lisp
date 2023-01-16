;;;; count-t.lisp

(in-package #:cl-reactive/tests)

(nst:def-test-group count-tests ()
  (nst:def-test simple-count-test (:equal 2)
    (signal-let (sig-x)
      (with-signal-values ((x sig-x) (y (signal-count sig-x)))
        (setf x t)
        (setf x nil)
        y)))

  (nst:def-test count-documentation-test (:equal "Yes")
    (signal-let (sig-x)
      (documentation (signal-count sig-x :documentation "Yes") t))))
