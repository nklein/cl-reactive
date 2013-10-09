;;;; with.lisp

(in-package #:cl-reactive)

(defun %listify-signal (sig)
  "Internal: ensure that SIG is a list.  If it is not, make it list of length two where both values are SIG."
  (etypecase sig
    (symbol (list sig sig))
    (list   sig)))

(defun %signal-name (sig)
  "Internal: return the local name for a given signal clause SIG."
  (first sig))

(defun %signal (sig)
  "Internal: return the actual signal from a given signal clause SIG."
  (second sig))

(defun %signal-to-var (sig)
  "Internal: create a gensym based on the local name for the given signal clause SIG."
  (gensym (symbol-name (%signal-name sig))))

(defun %signal-to-let-binding (sig var)
  "Internal: create a LET binding which holds the actual signal from the signal clause SIG in the given variable VAR."
  `(,var ,(%signal sig)))

(defun %signal-to-symbol-macrolet (sig var)
  "Internal: create a SYMBOL-MACROLET binding for the local name from the given signal clause SIG to use the SIGNAL-VALUE of the given VAR."
  `(,(%signal-name sig) (signal-value ,var)))

(defmacro with-signal-values ((&rest signals) &body body)
  "Locally bind the values of the listed SIGNALS and then execute the given BODY.  The SIGNALS list is a series of clauses of the form (LOCAL-NAME EXTERNAL-NAME).  The LOCAL-NAME is the name the signal value will be bound to during the execution of BODY.  The EXTERNAL-NAME is the variable holding the dependent signal.  One can specify only the EXTERNAL-NAME if the LOCAL-NAME will be the same, however as the LOCAL-NAME is bound using SYMBOL-MACROLET, it cannot be the name of a special variable."
  (let* ((signals (mapcar #'%listify-signal signals))
         (vars (mapcar #'%signal-to-var signals)))
    `(let ,(mapcar #'%signal-to-let-binding signals vars)
       (declare (ignorable ,@vars))
       (symbol-macrolet ,(mapcar #'%signal-to-symbol-macrolet signals vars)
         ,@body))))
