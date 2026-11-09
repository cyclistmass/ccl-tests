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
