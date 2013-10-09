;;;; let.lisp

(in-package #:cl-reactive)

(defun %decl-to-let-binding (decl)
  (etypecase decl
    (symbol
     `(,decl (signal-variable nil)))
    (list
     (destructuring-bind (name value
                               &key
                                 (type nil typep)
                                 (documentation nil documentationp)) decl
       `(,name (signal-variable ,value
                                ,@(when typep
                                    `(:type ',type))
                                ,@(when documentationp
                                    `(:documentation ,documentation))))))))

(defmacro signal-let ((&rest decls) &body body)
  "Create the local signals specified in DECLS and then execute the given BODY.  Here, DECLS is a list of LET-like declarations.  Each declaration in DECLS has the form: (NAME VALUE &KEY TYPE DOCUMENTATION).  The NAME is the name to bind the signal to.  The VALUE is the initial value of the signal.  The TYPE is the type-specifier for the signal values.  The DOCUMENTATION is a string used to document this signal.  If a declaration is simply a symbol, it is used as a NAME with a NIL value and the default type with no documentation."
  `(let ,(mapcar #'%decl-to-let-binding decls)
     ,@body))
