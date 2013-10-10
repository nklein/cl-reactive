;;;; on-change.lisp

(in-package #:cl-reactive)

(defun signal-on-change (sig &key
                               (test #'equal)
                               (key #'identity)
                               documentation)
  "Create a signal function that returns the value of SIG but that only triggers dependents to update when its value has actually changed.  The given TEST function is used to tell if two values are equal.  The KEY function is called on the old value and new value and the results are passed to the TEST function to tell if the results are equal.  The default TEST is #'EQUAL and the default KEY is #'IDENTITY."
  (let* ((documentation (or documentation
                            (format nil "~A" `(ON-CHANGE ,sig))))
         (int-doc (concatenate 'string "Internal: " documentation))
         (value (funcall key (signal-value sig)))
         (sig-internal (signal-variable value
                                         :type (signal-type sig)
                                         :documentation int-doc)))
    (signal-flet ((sig-update ((v sig))
                              (let ((k (funcall key v)))
                                (unless (funcall test value k)
                                  (with-signal-values ((int sig-internal))
                                    (setf value k
                                          int v))))
                              v))
      ;; Here, it would be nice to just return sig-internal and
      ;; be done with it.  But, if we did that, then no one would
      ;; be referencing sig-update and the garbage collector will
      ;; reap it.  So, we artificially reference it here as a second
      ;; value for the signal-function which will never be looked at.
      (signal-function (lambda ()
                         (with-signal-values ((int sig-internal)
                                              (upd sig-update))
                           (values int upd)))
                       (list sig-internal)
                       :type (signal-type sig)
                       :documentation documentation))))
