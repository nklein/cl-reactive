;;;; types.lisp

(in-package #:cl-reactive)

(defun %raise-type-error (value place type)
  "Internal: Because PLACE's value VALUE is not of the given TYPE, raise a TYPE-ERROR with a restart that allows one to overwrite the VALUE."
  (let ((condition (make-condition 'type-error
                                   :datum value
                                   :expected-type type)))
    (restart-case
        (error condition)
      (store-value (new)
        :report (lambda (s)
                  (format s "Assign a new value of type ~A to ~S" type place))
        :interactive (lambda ()
                       (let ((s *query-io*))
                         (format s "~&New value for ~S :" place)
                         (force-output s)
                         (list (eval (read s)))))
        new))))

(defmacro check-value-type (place type)
  "Internal: Ensure that the PLACE has a value of the given TYPE.  If not, raise a recoverable error."
  (let ((value (gensym "VALUE-")))
    `(loop :for ,value = ,place
        :until (typep ,value ,type)
        :do (setf ,place (%raise-type-error ,value ',type ',place)))))
