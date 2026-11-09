;;; r0-dx-escape-signature.lisp -- deliberately violates dynamic-extent.
(in-package :cl-user)
(defvar *escaped* nil)
(defvar *old* (make-array 64 :initial-element nil))   ; will be tenured
(defvar *sink* nil)

(defun leak (victim)
  ;; spacer first (higher on the tstack), the leaked cons below it, so the
  ;; (gc) call's own tstack use re-zeroes the spacer region, not ours.
  (let ((spacer (make-list 4096 :initial-element 0)))
    (declare (dynamic-extent spacer))
    (let ((c (list victim victim)))
      (declare (dynamic-extent c))
      (setq *escaped* c)               ; the escape
      (+ (length spacer) (length c)))))

(defun run (mode)
  (when (member "check" ccl:*command-line-argument-list* :test #'equal)
    (setq ccl::*gc-event-status-bits* (logior ccl::*gc-event-status-bits* (ash 1 2))))
  (format t "~&R0 mode ~s check ~s~%" mode (logbitp 2 ccl::*gc-event-status-bits*))
  (finish-output)
  (ccl:gc)                             ; tenure *old*
  (dotimes (i 200)
    (let ((victim (make-array 7 :initial-element i)))   ; a uvector, 64 bytes
      (leak victim)))                  ; frame dies; victim unreachable
  ;; victims are garbage now; the dead frame still holds the last one's address
  (ccl:gc)                             ; frees/moves them; dead frame not forwarded
  (dotimes (i 20000)                   ; re-fill young space with conses/gvectors
    (push (cons i (make-array 3)) *sink*))
  (let ((stale (car *escaped*)))       ; read through the dead frame
    (ecase mode
      (:heap  (setf (svref *old* 0) (list stale)))     ; trace-2 shape: memoized root -> cons -> stale
      (:stack (let ((l (list stale stale)))            ; trace-1 shape: live DX frame -> stale
                (declare (dynamic-extent l))
                (ccl:gc)
                (length l)))))
  (ccl:gc)
  (format t "~&R0 SURVIVED (unexpected)~%")
  (ccl:quit 0))

(run (if (member "stack" ccl:*command-line-argument-list* :test #'equal) :stack :heap))
