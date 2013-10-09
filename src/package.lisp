;;;; package.lisp

(defpackage #:cl-reactive
  (:use #:cl #:trivial-garbage #:anaphora)
  (:export #:signal-value)
  (:export #:defsignal-variable)
  (:export #:defsignal-function)
  (:export #:with-signal-values)
  (:export #:signal-let)
  (:export #:signal-flet)
  (:export #:with-signal-updates-deferred)
  (:documentation "This package provides reactive functions which can depend on other reactive functions or signals."))