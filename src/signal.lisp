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
               :documentation "Internal: mutex used to protect the data members in this signal from simultaneous access in different threads.")
   (documentation :initarg :documentation :initform nil :reader signal-documentation
                  :documentation "Internal: documentation for this signal variable."))
  (:documentation "Internal: This class forms the base class for SIGNAL-VARIABLE and SIGNAL-FUNCTION classes.  It holds those items common to both.  SIGNAL-VARIABLE instances can have their SIGNAL-VALUEs set.  SIGNAL-FUNCTION instance can depend on SIGNAL-VALUEs or SIGNAL-FUNCTIONS.  A SIGNAL-FUNCTION instance is triggered when a SIGNAL-VALUE it depends (directly or transitively) changes."))

(defun signalp (x)
  "Predicate to test whether an object is a SIGNAL."
  (typep x 'signal-base))

(defmacro with-signal-locked ((sig) &body body)
  "Internal: Execute the given BODY with the mutex of the signal SIG locked."
  `(bt:with-lock-held ((signal-mutex ,sig))
     ,@body))

(defmethod documentation ((sig signal-base) (type (eql t)))
  (signal-documentation sig))

(defmethod print-object ((sig signal-base) stream)
  "Internal: print the SIGNAL-BASE object SIG to the given STREAM."
  (if *print-readably*
      (call-next-method)
      (print-unreadable-object (sig stream :type t :identity t)
        (format stream "~S ~A"
                (with-signal-locked (sig)
                  (if (slot-boundp sig 'value)
                      (%signal-value sig)
                      :<unbound>))
                (signal-type sig)))))
