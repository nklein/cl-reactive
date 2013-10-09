;;;; on-change-t.lisp

(in-package #:cl-reactive-tests)

(nst:def-test-group on-change-tests ()
  (nst:def-test simple-on-change-test (:values (:equal 2) (:equal 3))
    (signal-let ((sig-x nil :type boolean))
      (with-signal-values ((x sig-x)
                           (changes (signal-count (signal-on-change sig-x)))
                           (raw-count (signal-count sig-x)))
        (setf x t)
        (setf x t)
        (setf x nil)
        (values changes raw-count))))

  (nst:def-test on-change-documentation-test (:equal "Yes")
    (signal-let (sig-x)
      (documentation (signal-on-change sig-x :documentation "Yes") t)))

  (nst:def-test on-change-with-test-test (:values (:equal 2) (:equal 3))
    (signal-let ((sig-x 0 :type integer))
      (flet ((mod5= (a b)
               (zerop (mod (- a b) 5))))
        (with-signal-values ((x sig-x)
                             (mod5-changes (signal-count
                                            (signal-on-change sig-x
                                                              :test #'mod5=)))
                             (all-changes (signal-count
                                           (signal-on-change sig-x))))
          (setf x 1
                x 2
                x 7)
          (values mod5-changes all-changes))))))
