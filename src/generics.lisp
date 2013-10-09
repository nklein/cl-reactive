;;;; generics.lisp

(in-package #:cl-reactive)

(defgeneric signal-value (sig)
  (:method (sig) sig)
  (:method ((sig function)) (funcall sig))
  (:documentation "Retrieve the current value of the signal SIG."))

(defgeneric (setf signal-value) (value sig)
  (:documentation "Set the current value of the signal SIG."))

(defgeneric signal-type (sig)
  (:method (sig) (type-of sig))
  (:method ((fn function)) t)
  (:documentation "Retrieve the type specifier for the given signal"))

(defgeneric add-signal-dependent (sig dep)
  (:method (sig dep) (declare (ignore sig dep)) nil)
  (:documentation "Internal:  Add DEP to the list of signals which depend on the signal SIG."))

(defgeneric remove-signal-dependent (sig dep)
  (:method (sig dep) (declare (ignore sig dep)) nil)
  (:documentation "Internal:  Remove DEP from the list of signals which depend on the signal SIG."))

(defgeneric mark-signal-dirty (sig)
  (:documentation "Internal: Mark the given signal SIG and signals that depend upon it as dirty.  Dirty will be recalculated next time they are accessed.  At the moment, unless inside a WITH-SIGNAL-UPDATES-DEFERRED section, SIG will be accessed again immediately after this call.  If inside a WITH-UPDATES-DEFERRED section, SIG will be accessed at the end of the section."))
