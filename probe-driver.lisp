;;; Fork CI driver, not for upstream: load the suite infrastructure and
;;; ccl-tests/stress.lisp, run LOCK-FREE-SHRINK-PROBE-KEYS, exit 0 on NIL.
(load (merge-pathnames "load.lisp" *load-truename*))
(load-tests :ansi nil :ccl nil :stress t)
(let ((r (funcall (find-symbol "LOCK-FREE-SHRINK-PROBE-KEYS" "CL-TEST"))))
  (format t "~&PROBE-RESULT: ~:[PASS~;FAIL~] ~s~%GCS ~s~%" r r
          (multiple-value-list (ccl:gccounts)))
  (finish-output)
  (ccl:quit (if r 1 0)))
