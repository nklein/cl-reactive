;;;; if-t.lisp

(in-package #:cl-reactive/tests)

(nst:def-test-group if-tests ()
  (nst:def-test simple-if-test (:values (:equal :yes) (:equal :no))
    (signal-let ((sig-cond t   :type boolean)
                 (sig-yes :yes :type symbol)
                 (sig-no  :no  :type symbol))
      (with-signal-values ((c sig-cond)
                           (i (signal-if sig-cond sig-yes sig-no)))
        (let ((a i))
          (setf c nil)
          (values a i)))))

  (nst:def-test if-documentation-test (:equal "Yes")
    (signal-let (sig-x)
      (documentation (signal-count sig-x :documentation "Yes") t))))
