;;;; variables-t.lisp

(in-package #:cl-reactive/tests)

(defsignal-variable *sig-x* nil
  :documentation "General signal used in variable tests.")
(defsignal-variable *sig-x-int* 0 :type integer
  :documentation "Integer-valued signal used in variable tests.")

(nst:def-test-group variable-tests ()
  (nst:def-test declare-returns-symbol (:equal '*sig-x-test*)
    (defsignal-variable *sig-x-test* :x))

  (nst:def-test declare-keeps-documentation (:true)
    (documentation *sig-x* t))

  (nst:def-test signal-variables-are-signals (:true)
    (signalp *sig-x*))

  (nst:def-test declare-then-query (:equal :x)
    (progn
      (with-signal-values ((x *sig-x*))
        (setf x :x))
      (with-signal-values ((xx *sig-x*))
        xx)))

  (nst:def-test declare-then-set-then-query (:equal t)
    (progn
      (with-signal-values ((x *sig-x*))
        (setf x :x))
      (with-signal-values ((xx *sig-x*))
        (setf xx t)
        xx)))

  (nst:def-test declare-enforces-type (:err :type type-error)
    (defsignal-variable *sig-x-int-err* 2/3 :type integer))

  (nst:def-test declare-setter-enforces-type (:err :type type-error)
    (progn
      (with-signal-values ((x *sig-x-int*))
        (setf x 2/3))))

  (nst:def-test with-then-query (:equal :x)
    (signal-let ((sig-x :x))
      (with-signal-values ((x sig-x))
        x)))

  (nst:def-test with-then-set-then-query (:equal t)
    (signal-let ((sig-x :x))
      (with-signal-values ((x sig-x))
        (setf x t)
        x)))

  (nst:def-test constructor-enforces-type (:err :type type-error)
    (signal-let ((sig-x 2/3 :type integer))
      (with-signal-values ((x sig-x))
        x)))

  (nst:def-test with-setter-enforces-type (:err :type type-error)
    (signal-let ((sig-x 0 :type integer))
      (with-signal-values ((x sig-x))
        (setf x 2/3)))))
