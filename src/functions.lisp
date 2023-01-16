;;;; functions.lisp

(in-package #:cl-reactive)

(defclass signal-function (signal-base #-ccl function)
  ((func       :initarg :func       :reader   signal-func
               :documentation "Internal: the function used to calculate this signal's value from the values of the signals it depends upon."))
  (:documentation "Internal: A SIGNAL-FUNCTION is a slot whose value is calculated as needed from the values of other signals it depends upon.  The signals can be either SIGNAL-VARIABLE instances or SIGNAL-FUNCTION instances.")
  (:metaclass closer-mop:funcallable-standard-class))

(defmethod initialize-instance :after ((sig signal-function)
                                       &key
                                         depends-on
                                       &allow-other-keys)
  "Internal: After initializing the SIGNAL-FUNCTION instance SIG, register its dependencies and prepare a finalization method that will remove those dependencies when the function is garbage collected."
  (let ((depends-on (copy-seq depends-on)))
    (dolist (dependee depends-on)
      (add-signal-dependent dependee sig))

    (flet ((funcallable-function ()
             (signal-value sig)))
      (closer-mop:set-funcallable-instance-function sig #'funcallable-function))

    (finalize sig (lambda ()
                    (dolist (dependee depends-on)
                      (remove-signal-dependent dependee sig))))))

(trace remove-signal-dependent)

(defun signal-function (function depends-on &key (type t) documentation)
  "Internal: create a SIGNAL-FUNCTION instance with the given FUNCTION which depends on the list of signals DEPENDS-ON, has the given TYPE, and the given DOCUMENTATION."
  (make-instance 'signal-function
                 :func function
                 :depends-on depends-on
                 :type type
                 :documentation documentation))

(defmethod signal-value ((func signal-function))
  "Internal: Return the current value of the SIGNAL-FUNCTION instance FUNC.  If this value needs to be recalculated, do it now."
  (with-signal-locked (func)
    (unless (slot-boundp func 'value)
      (let ((value (funcall (signal-func func))))
        (check-value-type value (signal-type func))
        (setf (%signal-value func) value)))
    (%signal-value func)))

(defmacro defsignal-function (name depends &body body)
  "Define a new calculated signal called NAME which depends on the given DEPENDS list and uses the given BODY to calculate this signal's value from the dependencies.  The DEPENDS list is a series of clauses of the form (LOCAL-NAME EXTERNAL-NAME).  The LOCAL-NAME is the name the signal value will be bound to during the execution of BODY.  The EXTERNAL-NAME is the variable holding the dependent signal.  One can specify only the EXTERNAL-NAME if the LOCAL-NAME will be the same, however as the LOCAL-NAME is bound using SYMBOL-MACROLET, it cannot be the name of a special variable."
  (let ((name (if (listp name)
                  name
                  (list name)))
        (sig (gensym "SIG-")))
    (destructuring-bind (just-name &key
                                   type
                                   (documentation nil documentationp)) name
      (declare (ignore type))
      (when (and (not documentationp)
                 (stringp (first body))
                 (rest body))
        (setf documentationp t
              documentation (first body)
              name (append name `(:documentation ,documentation))))
      (when (and (stringp (first body))
                 (rest body))
        (setf body (rest body)))
      `(progn
         (defvar ,just-name (signal-flet (((,sig ,@(rest name)) ,depends
                                           ,@body))
                              ,sig))
         (defun ,just-name ()
           ,@(when documentationp
               `(,documentation))
           (signal-value ,just-name))))))
