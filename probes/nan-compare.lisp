;;; nan-compare.lisp -- Clozure/ccl#440 on this lisp: open-coded double-float
;;; compares against a quiet NaN, with traps off and with :invalid on.
(in-package :cl-user)
(defun lt2 (x y) (declare (type double-float x y) (optimize (speed 3) (safety 0))) (< x y))
(defun le2 (x y) (declare (type double-float x y) (optimize (speed 3) (safety 0))) (<= x y))
(defun eq2 (x y) (declare (type double-float x y) (optimize (speed 3) (safety 0))) (= x y))
(defun ne2 (x y) (declare (type double-float x y) (optimize (speed 3) (safety 0))) (/= x y))
(defun gt2 (x y) (declare (type double-float x y) (optimize (speed 3) (safety 0))) (> x y))
(defun ge2 (x y) (declare (type double-float x y) (optimize (speed 3) (safety 0))) (>= x y))
(defparameter *saved* (ccl:get-fpu-mode))
(ccl:set-fpu-mode :invalid nil :overflow nil :division-by-zero nil :underflow nil :inexact nil)
(defparameter *nan* (/ 0d0 0d0))
(format t "~&NAN-RESULT traps-off (< <= = /= > >=) NaN vs 1: ~s  (correct: (NIL NIL NIL T NIL NIL))~%"
        (list (lt2 *nan* 1d0) (le2 *nan* 1d0) (eq2 *nan* 1d0) (ne2 *nan* 1d0) (gt2 *nan* 1d0) (ge2 *nan* 1d0)))
(format t "~&NAN-RESULT traps-off (< <= = /= > >=) 1 vs NaN: ~s  (correct: (NIL NIL NIL T NIL NIL))~%"
        (list (lt2 1d0 *nan*) (le2 1d0 *nan*) (eq2 1d0 *nan*) (ne2 1d0 *nan*) (gt2 1d0 *nan*) (ge2 1d0 *nan*)))
(defun slt2 (x y) (declare (type single-float x y) (optimize (speed 3) (safety 0))) (< x y))
(defun sle2 (x y) (declare (type single-float x y) (optimize (speed 3) (safety 0))) (<= x y))
(defun seq2 (x y) (declare (type single-float x y) (optimize (speed 3) (safety 0))) (= x y))
(defun sne2 (x y) (declare (type single-float x y) (optimize (speed 3) (safety 0))) (/= x y))
(defun sgt2 (x y) (declare (type single-float x y) (optimize (speed 3) (safety 0))) (> x y))
(defun sge2 (x y) (declare (type single-float x y) (optimize (speed 3) (safety 0))) (>= x y))
(defparameter *snan* (/ 0f0 0f0))
(format t "~&NAN-RESULT single traps-off NaN vs 1: ~s~%"
        (list (slt2 *snan* 1f0) (sle2 *snan* 1f0) (seq2 *snan* 1f0) (sne2 *snan* 1f0) (sgt2 *snan* 1f0) (sge2 *snan* 1f0)))
(format t "~&NAN-RESULT single traps-off 1 vs NaN: ~s~%"
        (list (slt2 1f0 *snan*) (sle2 1f0 *snan*) (seq2 1f0 *snan*) (sne2 1f0 *snan*) (sgt2 1f0 *snan*) (sge2 1f0 *snan*)))
(defun blt (x y) (declare (type double-float x y) (optimize (speed 3) (safety 0))) (if (< x y) :yes :no))
(defun ble (x y) (declare (type double-float x y) (optimize (speed 3) (safety 0))) (if (<= x y) :yes :no))
(defun bgt (x y) (declare (type double-float x y) (optimize (speed 3) (safety 0))) (if (> x y) :yes :no))
(defun bge (x y) (declare (type double-float x y) (optimize (speed 3) (safety 0))) (if (>= x y) :yes :no))
(format t "~&NAN-RESULT branch traps-off (< <= > >=) NaN vs 1, 1 vs NaN: ~s ~s  (correct: all :NO)~%"
        (list (blt *nan* 1d0) (ble *nan* 1d0) (bgt *nan* 1d0) (bge *nan* 1d0))
        (list (blt 1d0 *nan*) (ble 1d0 *nan*) (bgt 1d0 *nan*) (bge 1d0 *nan*)))
(format t "~&NAN-RESULT branch traps-off ordered 1 vs 2 (< <= > >=): ~s  (correct: (:YES :YES :NO :NO))~%"
        (list (blt 1d0 2d0) (ble 1d0 2d0) (bgt 1d0 2d0) (bge 1d0 2d0)))
(format t "~&NAN-RESULT value ordered 1 vs 2, 2 vs 2 (< <= = /= > >=): ~s ~s~%"
        (list (lt2 1d0 2d0) (le2 1d0 2d0) (eq2 1d0 2d0) (ne2 1d0 2d0) (gt2 1d0 2d0) (ge2 1d0 2d0))
        (list (lt2 2d0 2d0) (le2 2d0 2d0) (eq2 2d0 2d0) (ne2 2d0 2d0) (gt2 2d0 2d0) (ge2 2d0 2d0)))
(format t "~&NAN-RESULT generic traps-off NaN vs 1: ~s~%"
        (mapcar (lambda (f) (funcall f *nan* 1d0)) '(< <= = /= > >=)))
(format t "~&NAN-RESULT generic traps-off 1 vs NaN: ~s~%"
        (mapcar (lambda (f) (funcall f 1d0 *nan*)) '(< <= = /= > >=)))
(ccl:set-fpu-mode :invalid t)
(format t "~&NAN-RESULT invalid-on (< NaN 1): ~s~%"
        (handler-case (list :returned (lt2 *nan* 1d0))
          (error (c) (list :signalled (type-of c)))))
(format t "~&NAN-RESULT generic (< NaN 1) invalid-on: ~s~%"
        (handler-case (list :returned (< *nan* 1d0))
          (error (c) (list :signalled (type-of c)))))
(format t "~&NAN-DISASM lt2:~%")
(disassemble 'lt2)
(finish-output)
(quit 0)
