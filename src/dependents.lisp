;;;; dependents.lisp

(in-package #:cl-reactive)

(defvar *defer-updates* nil
  "Internal: When this variable is NIL, marking a SIGNAL-FUNCTION as dirty causes it to be re-evaluated immediately.  When this is non-NIL list, marking a SIGNAL-FUNCTION as dirty adds the signal function to this list so that it can be evaluated later.")

(defmethod add-signal-dependent ((sig signal-base) (dep signal-function))
  "Internal: add the given SIGNAL-FUNCTION instance DEP as a dependent of the SIGNAL-BASE instance SIG."
  (with-signal-locked (sig)
    (pushnew (make-weak-pointer dep) (signal-dependents sig)
             :key #'weak-pointer-value
             :test #'eq)))

(defmethod remove-signal-dependent ((sig signal-base) dep)
  "Internal: remove the given DEP as a dependent of the SIGNAL-BASE instance SIG."
  (with-signal-locked (sig)
    (setf (signal-dependents sig)
          (remove dep (signal-dependents sig)
                  :key #'weak-pointer-value
                  :test #'eq))))

(defmethod mark-signal-dirty ((sig null))
  "Internal: If asked to mark dirty an expired weak-pointer, do nothing."
  0)

(defmethod mark-signal-dirty ((sig signal-base))
  "Internal: Given the SIGNAL-VARIABLE or SIGNAL-FUNCTION instance SIG, mark it and all of its dependents dirty.  Return the number of instances marked dirty."
  (let ((count 1))
    (dolist (dep-ref (with-signal-locked (sig)
                       (signal-dependents sig)))
      (incf count (mark-signal-dirty (weak-pointer-value dep-ref))))
    count))

(defmethod mark-signal-dirty :before ((sig signal-function))
  "Internal: Before marking a SIGNAL-FUNCTION instance SIG dirty, make its value slot unbound so that it will be recalculated when needed next."
  (with-signal-locked (sig)
    (slot-makunbound sig 'value)))

(defmethod mark-signal-dirty :after ((sig signal-function))
  "Internal: after marking a SIGNAL-FUNCTION instance SIG dirty, refresh its value.  If *DEFER-UPDATES* is non-NIL, add this SIG to the list of signals which need to be updated.  Otherwise, update this signal immediately."
  (cond
    (*defer-updates*
     (pushnew (make-weak-pointer sig) *defer-updates*))
    (t
     (ignore-errors
       (signal-value sig)))))

(defun %update-all-signals (signals)
  "SIGNALS is a list of weak pointers to signal instances to update.  Retrieve the value of each of the instances that is still active."
  (dolist (sig signals)
    (awhen (and sig (weak-pointer-value sig))
      (ignore-errors
        (signal-value it)))))

(defmacro with-signal-updates-deferred (() &body body)
  "Execute the BODY but do not eagerly recalculate signal values until BODY has finished.  If signal values are needed during BODY, they will be lazily recalculated."
  (let ((to-update (gensym "TO-UPDATE")))
    `(let (,to-update)
       (unwind-protect
            (let ((*defer-updates* '(nil)))
              (unwind-protect
                   (progn
                     ,@body)
                (setf ,to-update *defer-updates*)))
         (%update-all-signals ,to-update)))))
