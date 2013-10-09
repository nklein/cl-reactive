;;;; reduce-t.lisp

(in-package #:cl-reactive-tests)

(nst:def-test-group reduce-tests ()
  (nst:def-test simple-reduce-test (:equal 7)
    (signal-let ((sig-x 3 :type integer)
                 (sig-y 4 :type integer))
      (with-signal-values ((s (signal-reduce #'+ (list sig-x sig-y))))
        s)))

  (nst:def-test reduce-subseq-test (:equal '((#\! . #\c) . #\d))
    (signal-let ((sig-a #\a)
                 (sig-b #\b)
                 (sig-c #\c)
                 (sig-d #\d)
                 (sig-e #\e))
      (with-signal-values ((s (signal-reduce #'cons
                                             (list sig-a sig-b sig-c
                                                   sig-d sig-e)
                                             :start 2 :end 4
                                             :initial-value #\!)))
        s)))

  (nst:def-test reduce-from-end-subseq-test (:equal '(#\c #\d))
    (signal-let ((sig-a #\a)
                 (sig-b #\b)
                 (sig-c #\c)
                 (sig-d #\d)
                 (sig-e #\e))
      (with-signal-values ((s (signal-reduce #'cons
                                             (list sig-a sig-b sig-c
                                                   sig-d sig-e)
                                             :from-end t
                                             :start 2 :end 4
                                             :initial-value nil)))
        s)))

  (nst:def-test reduce-with-key-test (:equal 6)
    (signal-let ((sig-a 0)
                 (sig-b 1)
                 (sig-c 2))
      (with-signal-values ((s (signal-reduce #'*
                                             (list sig-a sig-b sig-c)
                                             :key #'1+)))
        s)))

  (nst:def-test reduce-documentation-test (:equal "Yes")
    (signal-let (sig-x)
      (documentation (signal-reduce #'+ (list sig-x)
                                    :documentation "Yes") t))))
