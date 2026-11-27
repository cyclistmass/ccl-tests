;;; KI-317 probes.  The Makefile test target loads this only after
;;; FORMAT.B.9 fails, in the same process, so load.lisp stays unchanged.

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
      ;; The inner LOOP of FORMAT.B.9, compiled, one step at a time.  Every
      ;; failing case runs it at least once; the cases where it runs zero
      ;; times are the ones that pass.
      (probe :inner-loop
             (lambda ()
               (funcall
                (compile nil
                         '(lambda (s2 neg)
                           (let ((len (length s2)) (ci+1 4))
                             (list
                              (loop for i from (if neg 2 1) below len
                                    always (if (= (mod (- len i) ci+1) 0)
                                               (eql (elt s2 i) #\,)
                                               (find (elt s2 i) "01")))
                              (loop for i from (if neg 2 1) below len
                                    collect (list i (mod (- len i) ci+1)
                                                  (char-code (elt s2 i))
                                                  (find (elt s2 i) "01")))
                              (map 'list #'char-code "01")))))
                "11" nil)))
      ;; Does the test still fail when run again in this process?
      (probe :rerun-failed
             (lambda ()
               (let ((out (with-output-to-string (*standard-output*)
                            (funcall (find-symbol "DO-TEST" "RT")
                                     (find-symbol "FORMAT.B.9" "CL-TEST")))))
                 (and (search "failed" out) t)))))
    (nreverse probes)))
