;;; Fork CI driver, not for upstream: load the suite infrastructure and
;;; ccl-tests/tests.lisp, run NAN-COMPARE-FAILURES, exit 0 on NIL.
(load (merge-pathnames "load.lisp" *load-truename*))
(load-tests :ansi nil :ccl t :stress nil)
(let ((r (funcall (find-symbol "NAN-COMPARE-FAILURES" "CL-TEST"))))
  (format t "~&RESULT: ~:[PASS~;FAIL~] ~d ~s~%" r (length r) (reverse r))
  (finish-output)
  (ccl:quit (if r 1 0)))
