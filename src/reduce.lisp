;;;; reduce.lisp

(in-package #:cl-reactive)

(defun signal-reduce (fn signals &key
                                   (from-end nil from-end-p)
                                   (start nil startp)
                                   (end nil endp)
                                   (initial-value nil initial-value-p)
                                   (key nil keyp)
                                   (type t)
                                   documentation)
  "Create a signal function of the given TYPE with the given DOCUMENTATION that returns the value of CL:REDUCE using the given function FN on the signal values of the signals in the list SIGNALS.  The signal function passes the FROM-END, START, END, INITIAL-VALUE, and KEY parameters along to CL:REDUCE."
  (let* ((signals (copy-seq signals))
         (documentation (or documentation
                            (format nil "~A" `(APPLY ,fn ,signals))))
         (function `(lambda ()
                      ,documentation
                      (reduce ,fn (mapcar #'signal-value ',signals)
                              ,@(when from-end-p `(:from-end ,from-end))
                              ,@(when startp `(:start ,start))
                              ,@(when endp `(:end ,end))
                              ,@(when initial-value-p
                                  `(:initial-value ,initial-value))
                              ,@(when keyp `(:key ,key))))))
    (signal-function (compile nil function)
                     signals
                     :type type
                     :documentation documentation)))
