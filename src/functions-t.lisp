;;;; functions-t.lisp

(in-package #:cl-reactive-tests)

(defvar *signal-x-count* 0)
(defsignal-function signal-x-count ((x *sig-x*))
  "Count the number of times *SIG-X* is updated."
  (incf *signal-x-count*))


(defsignal-function signal-y-test ((x *sig-x-int*)) x)

(defsignal-function signal-y-docstring ((x *sig-x-int*))
  "Yes"
  x)

(defsignal-function (signal-y-documentation :documentation "Yes")
    ((x *sig-x-int*))
  "No"
  x)

(nst:def-test-group signal-function-tests ()
  (nst:def-test declare-returns-symbol (:equal 'signal-y-test)
    (defsignal-function signal-y-test ((x *sig-x-int*)) x))

  (nst:def-test declare-retains-function-docstring (:equal "Yes")
    (documentation #'signal-y-docstring t))

  (nst:def-test declare-retains-signal-docstring (:equal "Yes")
    (documentation signal-y-docstring t))

  (nst:def-test declare-retains-function-documentation (:equal "Yes")
    (documentation #'signal-y-documentation t))

  (nst:def-test declare-retains-signal-documentation (:equal "Yes")
    (documentation signal-y-documentation t))

  (nst:def-test signal-x-count-called (:values (:equal 1) (:equal 1))
    (let ((*signal-x-count* 0))
      (with-signal-values ((x *sig-x*))
        (setf x t))
      (values *signal-x-count* (signal-x-count))))

  (nst:def-test signal-function-evaluates-once (:equal 1)
    (let ((calls 0))
      (with-signal-values ((x *sig-x-int*))
        (setf x 0))
      (signal-flet ((sig-y ((x *sig-x-int*)) (incf calls) x))
        (with-signal-values ((y sig-y))
          (+ y y)))
      calls))

  (nst:def-test signal-function-notices-updates (:equal 2)
    (signal-flet ((sig-y ((x *sig-x-int*)) x))
      (with-signal-values ((x *sig-x-int*) (y sig-y))
        (setf x 0)
        (+ y y)
        (setf x 1)
        (+ y y))))

  (nst:def-test signal-function-can-be-collected (:equal 0)
    (signal-let (sig-x)
      (signal-flet ((sig-y ((x sig-x)) x))
        (declare (ignore sig-y)))
      (trivial-garbage:gc :full t)
      (length (remove nil (mapcar #'trivial-garbage:weak-pointer-value
                                  (cl-reactive::signal-dependents sig-x)))))))
