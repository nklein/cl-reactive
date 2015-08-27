;;;; cl-reactive.asd

(asdf:defsystem #:cl-reactive
  :description "CL-REACTIVE is a reactive-programming package for Common Lisp."
  :author "Patrick Stein <pat@nklein.com>"
  :license "UNLICENSE"
  :depends-on (#:bordeaux-threads #:closer-mop #:trivial-garbage #:anaphora)
  :components
  ((:module "src"
    :components ((:file "package")
                 (:file "types" :depends-on ("package"))
                 (:file "generics" :depends-on ("package"))
                 (:file "signal" :depends-on ("package"))
                 (:file "variables" :depends-on ("package"
                                                 "types"
                                                 "generics"
                                                 "signal"))
                 (:file "functions" :depends-on ("package"
                                                 "types"
                                                 "generics"
                                                 "signal"))
                 (:file "dependents" :depends-on ("package"
                                                  "generics"
                                                  "signal"
                                                  "functions"))
                 (:file "with" :depends-on ("package"
                                            "variables"))
                 (:file "let" :depends-on ("package"
                                           "variables"
                                           "with"))
                 (:file "flet" :depends-on ("package"
                                            "variables"
                                            "functions"
                                            "let"))
                 (:file "count" :depends-on ("package"
                                             "flet"))
                 (:file "if" :depends-on ("package"
                                          "variables"
                                          "functions"))
                 (:file "on-change" :depends-on ("package"
                                                 "variables"
                                                 "functions"
                                                 "let"
                                                 "flet"))
                 (:file "apply" :depends-on ("package"
                                             "functions"))
                 (:file "reduce" :depends-on ("package"
                                              "variables"
                                              "functions"))))))

(asdf:defsystem #:cl-reactive-tests
  :description "Tests for the CL-REACTIVE package."
  :author "Patrick Stein <pat@nklein.com>"
  :license "UNLICENSE"
  :depends-on (#:cl-reactive #:nst)
  :components
  ((:module "src"
    :components ((:file "package-t")
                 (:file "variables-t" :depends-on ("package-t"))
                 (:file "functions-t" :depends-on ("package-t"))
                 (:file "with-t" :depends-on ("package-t"))
                 (:file "let-t" :depends-on ("package-t"))
                 (:file "flet-t" :depends-on ("package-t"
                                              "variables-t"))
                 (:file "dependents-t" :depends-on ("package-t"
                                                    "variables-t"))
                 (:file "count-t" :depends-on ("package-t"))
                 (:file "if-t" :depends-on ("package-t"))
                 (:file "on-change-t" :depends-on ("package-t"))
                 (:file "apply-t" :depends-on ("package-t"))
                 (:file "reduce-t" :depends-on ("package-t"))))))

(defmethod asdf:perform ((op asdf:test-op)
                         (system (eql (asdf:find-system :cl-reactive))))
  (asdf:load-system :cl-reactive-tests)
  (funcall (find-symbol (symbol-name :run-tests) :cl-reactive-tests)))
