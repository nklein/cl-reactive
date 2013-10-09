;;;; apply-t.lisp

(in-package #:cl-reactive-tests)

(nst:def-test-group apply-tests ()
  (nst:def-test simple-apply-test (:equal 5)
    (signal-let ((sig-x 3 :type integer)
                 (sig-y 4 :type integer))
      (flet ((hypotenuse (a b)
               (sqrt (+ (* a a) (* b b)))))
        (with-signal-values ((h (signal-apply #'hypotenuse
                                              (list sig-x sig-y))))
          h))))

  (nst:def-test apply-documentation-test (:equal "Yes")
    (signal-let (sig-x)
      (documentation (signal-apply #'+ (list sig-x) :documentation "Yes") t))))
