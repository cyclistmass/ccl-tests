;;; wait-for-signal-first-use.lisp -- threads that call WAIT-FOR-SIGNAL
;;; for the first time on the same signal must all be woken by it.
;;;
;;; The kernel makes the semaphore for a signal the first time a thread
;;; waits for that signal.  It tested for "no semaphore yet" and then
;;; stored a new one with no lock, so two threads that got there at once
;;; each made one.  The second store won, and the thread that made the
;;; first one waited on a semaphore that the signal handler never sees.
;;;
;;; For each of the real-time signals 44 to 64, the test starts 4
;;; threads that spin on a gate, opens the gate, and lets all 4 call
;;; (WAIT-FOR-SIGNAL sig 3) at once.  Then it sends the signal to the
;;; process 4 times.  On a correct lisp all 4 waits return T.  A wait
;;; that returns NIL timed out, so its thread waited on a lost semaphore.
;;; A signal that the lisp keeps for itself is skipped.
;;;
;;; The race is short, so a run can pass on a broken lisp; run the file
;;; more than once.  Linux only; other targets report a skip.
;;;
;;; expected-state: repro
;;;
;;; Contract: the file prints
;;;   WAIT-FOR-SIGNAL-FIRST-USE-RESULT :SIGNALS n :SKIPPED n :LOST n :VERDICT v
;;; and exits 0 only when v is PASS.

(in-package :cl-user)

(defparameter *threads* 4)
(defparameter *wait-seconds* 3)

(defun report (signals skipped lost verdict)
  (format t "~&WAIT-FOR-SIGNAL-FIRST-USE-RESULT :SIGNALS ~d :SKIPPED ~d :LOST ~d :VERDICT ~a~%"
          signals skipped lost verdict)
  (finish-output)
  (quit (if (equal verdict "PASS") 0 1)))

(unless (member :linux-target *features*)
  (report 0 0 0 "SKIP"))

(defun send-signal (sig)
  (ccl::external-call "kill" :int (ccl::getpid) :int sig :int))

(defun join (procs)
  "Wait for PROCS to finish, at most 2 * *WAIT-SECONDS*; kill any left."
  (let ((stop (+ (get-internal-real-time)
                 (* 2 *wait-seconds* internal-time-units-per-second))))
    (loop until (or (every #'process-exhausted-p procs)
                    (> (get-internal-real-time) stop))
          do (sleep 0.05))
    (dolist (p procs)
      (unless (process-exhausted-p p) (process-kill p)))))

(defun trial (sig)
  "Return :OK, :LOST n or :SKIP."
  (let* ((gate (list nil))
         (results (make-array *threads* :initial-element :running))
         (procs
           (loop for i below *threads*
                 collect (let ((i i))
                           (process-run-function
                            (format nil "waiter ~d" i)
                            (lambda ()
                              (loop until (car gate))
                              (setf (aref results i)
                                    (handler-case (wait-for-signal sig *wait-seconds*)
                                      (error () :error)))))))))
    (sleep 0.2)
    (setf (car gate) t)
    (sleep 0.3)
    (cond ((find :error results)
           ;; The lisp keeps this signal; no handler was installed, so
           ;; do not send it.  Let the other waits time out.
           (join procs)
           :skip)
          (t
           (loop repeat *threads*
                 do (send-signal sig)
                    (sleep 0.05))
           (join procs)
           (let ((lost (count-if-not (lambda (r) (eq r t)) results)))
             (if (zerop lost) :ok (list :lost lost)))))))

(let ((signals 0) (skipped 0) (lost 0))
  (loop for sig from 44 to 64
        for r = (trial sig)
        do (cond ((eq r :skip) (incf skipped))
                 (t (incf signals)
                    (when (consp r)
                      (incf lost (second r))
                      (format t "~&;; signal ~d: ~d wait(s) timed out~%" sig (second r))))))
  (report signals skipped lost
          (cond ((zerop signals) "NO-SIGNALS")
                ((plusp lost) "FAIL")
                (t "PASS"))))
