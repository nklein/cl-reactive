;;;; signal.lisp

(in-package #:cl-reactive)

(defclass signal-base ()
  ((value      :initarg  :value :accessor %signal-value
               :documentation "Internal: The current value of the signal.")
   (type       :initarg  :type  :reader   signal-type
               :documentation "Internal: The type for values of this signal.")
   (dependents :initform nil    :accessor signal-dependents
               :documentation "Internal: weak-pointers to signals which depend upon this one.")
   (mutex      :initform (bt:make-lock) :accessor signal-mutex
               :documentation "Internal: mutex used to protect the data members in this signal from simultaneous access in different threads."))
  (:documentation "Internal: This class forms the base class for SIGNAL-VARIABLE and SIGNAL-FUNCTION classes.  It holds those items common to both.  SIGNAL-VARIABLE instances can have their SIGNAL-VALUEs set.  SIGNAL-FUNCTION instance can depend on SIGNAL-VALUEs or SIGNAL-FUNCTIONS.  A SIGNAL-FUNCTION instance is triggered when a SIGNAL-VALUE it depends (directly or transitively) changes."))

(defmacro with-signal-locked ((sig) &body body)
  "Internal: Execute the given BODY with the mutex of the signal SIG locked."
  `(bt:with-lock-held ((signal-mutex ,sig))
     ,@body))

(defmethod initialize-instance :after ((sig signal-base)
                                       &key
                                         documentation
                                       &allow-other-keys)
  "Internal: after initializing a SIGNAL-BASE instance, set its documentation string if one was provided."
  (awhen documentation
    (setf (documentation sig t) it)))

(defmethod print-object ((sig signal-base) stream)
  "Internal: print the SIGNAL-BASE object SIG to the given STREAM."
  (if *print-readably*
      (call-next-method)
      (print-unreadable-object (sig stream :type t)
        (format stream ":VALUE ~A :TYPE ~A"
                (with-signal-locked (sig)
                  (if (slot-boundp sig 'value)
                      (%signal-value sig)
                      :<unbound>))
                (signal-type sig)))))
