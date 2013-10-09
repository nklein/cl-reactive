;;;; flet.lisp

(in-package #:cl-reactive)

(defun %fdecl-to-with-clause (decl)
  "Internal: Turn the DECL into a LET-binding for a SIGNAL-FUNCTION instance.  The DECL is of the form (NAME (&REST DEPENDS) &BODY BODY) where NAME is either a SYMBOL or a list of the form (NAME &KEY TYPE DOCUMENTATION).  The DEPENDS, TYPE (when provided), and DOCUMENTATION (when provided) are passed directly to SIGNAL-FUNCTION.  The resulting signal is locally bound with the given NAME.  The BODY is used to recalculate the value of this signal from the bound DEPENDS."
  (destructuring-bind (name (&rest depends) &body body) decl
    (destructuring-bind (name &key
                              (type t typep)
                              (documentation nil documentationp))
        (if (listp name)
            name
            (list name))
      `(,name (signal-function (lambda ()
                                 (with-signal-values ,depends
                                   ,@body))
                               (list ,@(mapcar #'%signal depends))
                               ,@(when typep
                                   `(:type ',type))
                               ,@(when documentationp
                                   `(:documentation ,documentation)))))))

(defmacro signal-flet ((&rest fdecls) &body body)
  "Create the local, calculated signals specified in FDECLS and then execute the given BODY.  Here, FDECLS is a list of FLET-like declarations.  Each declaration in FDECLS is of the form (NAME (&REST DEPENDS) &BODY SIG-BODY) where NAME is either a SYMBOL or a list of the form (NAME &KEY TYPE DOCUMENTATION).  The SIG-BODY is used to recalculate the value of this signal when its dependencies have changed.  The calculated signal will have values of the given TYPE.  The calculated signal will have the given DOCUMENTATION and be locally bound with the given NAME.  The DEPENDS list is a series of clauses of the form (LOCAL-NAME EXTERNAL-NAME).  The LOCAL-NAME is the name the signal value will be bound to during the execution of SIG-BODY.  The EXTERNAL-NAME is the variable holding the dependent signal.  One can specify only the EXTERNAL-NAME if the LOCAL-NAME will be the same, however as the LOCAL-NAME is bound using SYMBOL-MACROLET, it cannot be the name of a special variable."
  `(let ,(mapcar #'%fdecl-to-with-clause fdecls)
     ,@body))
