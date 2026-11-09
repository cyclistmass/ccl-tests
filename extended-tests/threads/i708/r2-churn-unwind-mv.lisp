;;; r2-churn-unwind-mv.lisp
(in-package :cl-user)
(defvar *duration* 90) (defvar *stop* nil) (defvar *bad* nil)
(defvar *old* (make-array 1024 :initial-element nil))
(defvar *special* 0)
(defun shape-ok (x) (and (consp x) (simple-vector-p (cdr x)) (eql (car x) (svref (cdr x) 0))))
(defun mv-dx (n)
  (let ((l (make-list n :initial-element n)))
    (declare (dynamic-extent l))
    (multiple-value-prog1 (values (make-array 2) (length l) (cons n (make-array 1 :initial-element n)))
      (progv '(*special*) (list n)              ; progvsave tstack frame
        (push (cons n (make-array 1 :initial-element n)) (svref *old* (mod n 1024)))))))
(defun body (seed)
  (let ((n 0))
    (dotimes (i 2000)
      (catch 'out
        (unwind-protect
             (let ((v (make-array (1+ (mod i 50)) :initial-element i)))
               (declare (dynamic-extent v))
               (multiple-value-bind (a b c) (mv-dx (1+ (mod i 200)))
                 (declare (ignore a b))
                 (setf (svref *old* (mod (+ seed i) 1024)) c))
               (when (oddp i) (throw 'out nil)))
          (setf (svref *old* (mod i 1024)) (cons i (make-array 1 :initial-element i)))))
      (incf n))
    n))
(defun checker ()
  (loop until *stop* do
    (dotimes (i 1024)
      (let ((x (svref *old* i)))
        (unless (or (null x) (shape-ok x) (and (consp x) (every #'shape-ok x)))
          (setq *bad* (format nil "slot ~d" i) *stop* t))))))
(defun run ()
  (setq ccl::*gc-event-status-bits*
        (logior ccl::*gc-event-status-bits* (ash 1 2)))
  (ccl:gc)
  (let ((chk (ccl:process-run-function "chk" #'checker))
        (end (+ (get-universal-time) *duration*)) (born 0))
    (loop until (or *stop* (> (get-universal-time) end)) do
      ;; 8 short-lived threads at a time; each dies while others are mid-GC
      (let ((ps (loop for k below 8 collect
                  (ccl:process-run-function (format nil "t~d" k) (lambda () (body k))))))
        (dolist (p ps) (ccl:process-interrupt p (lambda () (mv-dx 16))))   ; interrupt during DX/MV
        (mapc #'ccl:join-process ps) (incf born 8)))
    (setq *stop* t) (ccl:join-process chk)
    (format t "~&R2 :VERDICT ~a :THREADS ~d :GCS ~a~%" (if *bad* "BAD-SHAPE" "PASS") born (ccl:gccounts))
    (ccl:quit (if *bad* 1 0))))
(run)
