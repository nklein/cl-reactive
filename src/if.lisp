;;;; if.lisp

(in-package #:cl-reactive)

(defun signal-if (sig-cond sig-true sig-false &key documentation)
  "Create a signal function that returns the value of SIG-TRUE when SIG-COND's value is non-NIL and the value of SIG-FALSE otherwise."
  (let ((documentation (or documentation
                           (format nil "~A" `(IF ,sig-cond
                                                 ,sig-true
                                                 ,sig-false)))))
    (signal-function (lambda ()
                       (with-signal-values ((c sig-cond)
                                            (y sig-true)
                                            (n sig-false))
                         (if c y n)))
                     (list sig-cond sig-true sig-false)
                     :type `(or ,(signal-type sig-true)
                                ,(signal-type sig-false))
                     :documentation documentation)))
