;;; Probe, not a test: what happens when a storage-condition handler
;;; overflows the control stack again, on the stack that overflowed.

(defun deep (n) (if (zerop n) 0 (1+ (deep (1- n)))))

(defvar *log* nil)
(defvar *lock* (make-lock))

(defun note (x)
  (with-lock-grabbed (*lock*) (push x *log*))
  (format t "~&NOTE ~s~%" x)
  (finish-output))

(defun body ()
  (note :start)
  (let ((r (catch :outer
             (handler-case
                 (handler-bind ((storage-condition
                                  (lambda (c)
                                    (declare (ignore c))
                                    (note :first-overflow)
                                    (deep most-positive-fixnum)
                                    (note :inner-returned))))
                   (deep most-positive-fixnum))
               (storage-condition () (note :caught-outside) :caught)))))
    (note (list :body-returned r))))

(let* ((p (process-run-function "overflow" #'body))
       (done (process-wait-with-timeout "join" (* 60 *ticks-per-second*)
                                        (lambda () (process-exhausted-p p)))))
  (note (list :joined done))
  ;; Is the image still usable after the thread is gone?
  (note (list :alive (deep 1000)))
  (format t "~&RESULT ~s~%" (reverse *log*))
  (finish-output)
  (quit (if done 0 1)))
