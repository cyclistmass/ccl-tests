;;; KI-317 probe.  FORMAT.B.9 fails now and then on 32-bit x86: once it
;;; fails, almost every case fails, even "11", though the strings are right.
;;; Run the whole suite.  If FORMAT.B.9 failed, test the operations its
;;; predicate uses, in the same process, and print one RESULT line.
;;; Exit 1 when FORMAT.B.9 failed, so the CI tally counts it.

;;; Delete the test fasls first, as `make test' does, so this process
;;; compiles every test file before it runs them.
(let ((fasls (directory
              (merge-pathnames
               (make-pathname :directory '(:relative :up "ansi-tests" :wild-inferiors)
                              :name :wild
                              :type (pathname-type ccl:*.fasl-pathname*))
               *load-truename*))))
  (mapc #'delete-file fasls)
  (format t "~&KI317 deleted ~d fasls~%" (length fasls)))

(load (merge-pathnames (make-pathname :directory '(:relative :up)
                                      :name "load" :type "lisp")
                       *load-truename*))

(defun ki317-probes ()
  (let ((probes '()))
    (flet ((probe (name thunk)
             (push (cons name
                         (handler-case (funcall thunk)
                           (error (c) (list :error (princ-to-string c)))))
                   probes)))
      ;; Literal strings, interpreted.
      (probe :lit-string= (lambda () (string= "11" "11")))
      (probe :lit-remove (lambda () (remove #\, "11")))
      (probe :lit-string=-remove (lambda () (string= "11" (remove #\, "11"))))
      (probe :lit-elt (lambda () (elt "11" 0)))
      (probe :lit-find (lambda () (find #\1 "01")))
      (probe :lit-mod (lambda () (mod 1 4)))
      ;; Strings from FORMAT, as the test makes them.
      (let ((s1 (format nil "~b" 3))
            (s2 (format nil "~:B" 3)))
        (probe :fmt-types (lambda () (list (type-of s1) (type-of s2))))
        (probe :fmt-string= (lambda () (string= s1 s2)))
        (probe :fmt-remove (lambda () (remove #\, s2)))
        (probe :fmt-string=-remove (lambda () (string= s1 (remove #\, s2))))
        (probe :fmt-elt (lambda () (list (elt s2 0) (elt s2 1))))
        (probe :fmt-find (lambda () (find (elt s2 1) "01"))))
      ;; The same, compiled, as DO-TESTS :COMPILE T does.
      (probe :compiled
             (lambda ()
               (funcall
                (compile nil
                         '(lambda (i)
                           (let* ((s1 (format nil "~b" i))
                                  (s2 (format nil "~:B" i)))
                             (list (string= s1 (remove #\, s2))
                                   (remove #\, s2)
                                   (not (eql (elt s2 0) #\,))
                                   (find (elt s2 1) "01")
                                   (mod (- (length s2) 1) 4)))))
                3)))
      ;; Does the test still fail when run again in this process?
      (probe :rerun-failed
             (lambda ()
               (let ((out (with-output-to-string (*standard-output*)
                            (funcall (find-symbol "DO-TEST" "RT")
                                     (find-symbol "FORMAT.B.9" "CL-TEST")))))
                 (and (search "failed" out) t)))))
    (nreverse probes)))

;;; Run the suite as a toplevel -e form would.  LOAD binds the pathnames to
;;; this file and opens a compilation unit, so the LOAD-TRUENAME tests and
;;; the compiler-warning tests would fail.
(let* ((failed (let ((*load-pathname* nil)
                     (*load-truename* nil)
                     (ccl::*outstanding-deferred-warnings* nil))
                 (run-tests)))
       (b9 (find "FORMAT.B.9" failed :key #'symbol-name :test #'string=)))
  (format t "~&KI317 failed tests: ~s~%" failed)
  (cond (b9
         (format t "~&RESULT b9=FAIL ~s~%" (ki317-probes))
         (finish-output)
         (ccl:quit 1))
        (t
         (format t "~&RESULT b9=pass other-failed=~d~%" (length failed))
         (finish-output)
         (ccl:quit 0))))
