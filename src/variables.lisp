;;;; variables.lisp

(in-package #:cl-reactive)

(defclass signal-variable (signal-base)
  ()
  (:documentation "Internal: A SIGNAL-VARIABLE is effectively a slot for a value of a given type.  One can set or retrieve the value of the SIGNAL-VARIABLE.  SIGNAL-FUNCTION instances can depend on the SIGNAL-VARIABLE.  When the SIGNAL-VARIABLE is set, it triggers all SIGNAL-FUNCTION instance which depend upon it (directly or transitively) to recalculate their values."))

(defun signal-variable (value &key
                                (type t)
                                documentation)
  "Internal: create a SIGNAL-VARIABLE instance with the given VALUE, TYPE, and DOCUMENTATION."
  (check-value-type value type)
  (make-instance 'signal-variable
                 :value value
                 :type type
                 :documentation documentation))

(defmethod signal-value ((var signal-variable))
  "Internal: Return the current value of the SIGNAL-VARIABLE instance SIG."
  (%signal-value var))

(defmethod (setf signal-value) (value (var signal-variable))
  "Internal: Set the current value of the SIGNAL-VARIABLE instance SIG to VALUE.  Throw a TYPE-ERROR with a restart if the VALUE is not the correct type for SIG."
  (with-signal-locked (var)
    (check-value-type value (signal-type var))
    (setf (%signal-value var) value)))

(defmethod (setf signal-value) :after (value (sig signal-variable))
  "Internal: after a SIGNAL-VALUE is set on the SIGNAL-VARIABLE object SIG, mark SIG's dependents as dirty to trigger recalculations."
  (declare (ignore value))
  (mark-signal-dirty sig))

(defmacro defsignal-variable (name value &key (type t typep)
                                           (documentation nil documentationp))
  "Create a new signal variable called NAME with the given VALUE, TYPE, and DOCUMENTATION.  If VALUE is not of the given TYPE, a TYPE-ERROR will be thrown with a restart allowing one to replace the VALUE."
  `(defvar ,name (signal-variable ,value
                                  ,@(when typep
                                          `(:type ',type))
                                  ,@(when documentationp
                                          `(:documentation ,documentation)))
     ,@(when documentationp
         `(,documentation))))
