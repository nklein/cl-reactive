;;;; apply.lisp

(in-package #:cl-reactive)

(defun signal-apply (fn signals &key
                                  (type t)
                                  documentation)
  "Create a signal function of the given TYPE with the given DOCUMENTATION that returns the value of FN applied to the value of the signals in the list SIGNALS."
  (let* ((documentation (or documentation
                            (format nil "~A" `(APPLY ,fn ,signals)))))
    (signal-function (lambda ()
                       (apply fn (mapcar #'signal-value signals)))
                     signals
                     :type type
                     :documentation documentation)))
