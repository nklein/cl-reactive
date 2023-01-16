;;;; with-t.lisp

(in-package #:cl-reactive/tests)

(nst:def-test-group with-signals-tests ()
  (nst:def-test let-then-query (:equal :x)
    (signal-let ((sig-m :x))
      (with-signal-values ((m sig-m))
        m)))

  (nst:def-test let-then-rename-query (:equal :x)
    (signal-let ((sig-m :x))
      (with-signal-values ((n sig-m))
        n)))

  (nst:def-test let-then-query-same-name (:equal :x)
    (signal-let ((m :x))
      (with-signal-values (m)
        m)))

  (nst:def-test let-then-set-then-query (:equal t)
    (signal-let ((sig-m :x))
      (with-signal-values ((m sig-m))
        (setf m t)
        m)))

  (nst:def-test let-then-set-then-query-same-name (:equal t)
    (signal-let ((m :x))
      (with-signal-values (m)
        (setf m t)
        m))))
