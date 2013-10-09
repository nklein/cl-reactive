;;;; generics.lisp

(in-package #:cl-reactive)

(defgeneric signal-value (sig)
  (:documentation "Retrieve the current value of the signal SIG."))
(defgeneric (setf signal-value) (value sig)
  (:documentation "Set the current value of the signal SIG."))

(defgeneric add-signal-dependent (sig dep)
  (:documentation "Internal:  Add DEP to the list of signals which depend on the signal SIG."))
(defgeneric remove-signal-dependent (sig dep)
  (:documentation "Internal:  Remove DEP from the list of signals which depend on the signal SIG."))

(defgeneric mark-signal-dirty (sig)
  (:documentation "Internal: Mark the given signal SIG and signals that depend upon it as dirty.  Dirty will be recalculated next time they are accessed.  At the moment, unless inside a WITH-SIGNAL-UPDATES-DEFERRED section, SIG will be accessed again immediately after this call.  If inside a WITH-UPDATES-DEFERRED section, SIG will be accessed at the end of the section."))
