;;; code-executable-gc-race.lisp -- a GC that suspends a thread inside
;;; %make-code-executable must find a well-formed control stack.
;;;
;;; On arm64, %make-code-executable built the c-frame for its foreign
;;; call by moving sp first and storing the frame header two instructions
;;; later.  A GC that suspended the thread in between walked the control
;;; stack from sp and found whatever an earlier, deeper call had left
;;; there instead of a header.
;;;
;;; One thread calls %make-code-executable from a varying stack depth, so
;;; that the stale word under sp is different each time.  Another thread
;;; collects as fast as it can.  A correct lisp runs until the deadline
;;; and exits 0.  A broken one dies in the kernel or never finishes, and
;;; only the external timeout sees that.
;;;
;;; x86-64 has no %make-code-executable, and the file reports a skip.
;;;
;;; Contract:  CODE-EXECUTABLE-GC-RACE-RESULT :CALLS n :GCS n :VERDICT v

(in-package :cl-user)

(defparameter *seconds* 60
  "How long the two threads race.")

(defvar *stop* nil)
(defvar *calls* 0)
(defvar *gcs* 0)

(defun report (verdict)
  (format t "~&CODE-EXECUTABLE-GC-RACE-RESULT :CALLS ~d :GCS ~d :VERDICT ~a~%"
          *calls* *gcs* verdict)
  (finish-output))

(defun at-depth (n v)
  ;; Not a tail call: each level leaves its frame on the stack.
  (if (zerop n)
      (progn (funcall 'ccl::%make-code-executable v) 0)
      (1+ (at-depth (1- n) v))))

(defun caller ()
  (let ((v (make-array 4 :element-type '(unsigned-byte 32) :initial-element 0)))
    (loop for i from 0
          until *stop*
          do (at-depth (mod (* i 7) 41) v)
             (incf *calls*))))

(defun collector ()
  (loop until *stop*
        do (gc)
           (incf *gcs*)))

(unless (fboundp 'ccl::%make-code-executable)
  (report "SKIP")
  (quit 0))

(let ((a (process-run-function "caller" #'caller))
      (b (process-run-function "collector" #'collector)))
  (sleep *seconds*)
  (setq *stop* t)
  (process-wait "caller" #'process-exhausted-p a)
  (process-wait "collector" #'process-exhausted-p b)
  (report "PASS")
  (quit 0))
