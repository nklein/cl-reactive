;;;; count.lisp

(in-package #:cl-reactive)

(defun signal-count (sig &key documentation)
  "Create a signal function that returns the number of times the signal SIG has been updated.  Note:  This count actually reflects the number of times that a signal function dependent on SIG is updated so if SIG is updated multiple times within a single WITH-SIGNAL-UPDATES-DEFERRED section, this count would likely not reflect all of those updates."
  (let ((count 0)
        (documentation (or documentation
                           (format nil "Count updates to ~A" sig))))
    (signal-flet (((count-sig :type (integer 0 *)
                              :documentation documentation) ((x sig))
                    (declare (ignore x))
                    (incf count)))
      count-sig)))
