;;;; flet-t.lisp

(in-package #:cl-reactive-tests)

(nst:def-test-group signal-flet-tests ()
  (nst:def-test flet-then-query (:equal 1)
    (signal-let ((sig-x 0 :type integer))
      (signal-flet ((sig-y ((x sig-x)) (1+ x)))
        (with-signal-values ((y sig-y))
          y))))

  (nst:def-test flet-then-query-with-global (:equal 1)
    (progn
      (with-signal-values ((x *sig-x-int*))
        (setf x 0))
      (signal-flet ((sig-y ((x *sig-x-int*)) (1+ x)))
        (with-signal-values ((y sig-y))
          y))))

  (nst:def-test nested-flet-then-query (:equal 2)
    (signal-let ((sig-x 0 :type integer))
      (signal-flet ((sig-y ((x sig-x)) (1+ x)))
        (signal-flet ((sig-z ((y sig-y)) (1+ y)))
          (with-signal-values ((z sig-z))
            z)))))

  (nst:def-test without-lexical-scope (:equal 1)
    (flet ((make-signal (sig-x)
             (signal-flet ((sig-y ((x sig-x)) (1+ x)))
               sig-y)))
      (let ((sig-y (signal-let ((sig-x 0 :type integer))
                     (make-signal sig-x))))
        (with-signal-values ((y sig-y))
          y))))

  (nst:def-test flet-keeps-documentation (:equal "Yes")
    (signal-flet (((sig-y :documentation "Yes") ((x *sig-x*)) x))
      (documentation sig-y t)))

  (nst:def-test flet-enforces-type (:err :type type-error)
    (signal-flet (((sig-y :type integer) ((x *sig-x-int*)) (+ x 1/2)))
      (with-signal-values ((y sig-y))
        y))))
