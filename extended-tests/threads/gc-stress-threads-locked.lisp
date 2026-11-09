;;; gc-stress-threads-locked.lisp -- gc-stress-threads.lisp with every
;;; store and read of an item under a lock.  The lock orders the stores
;;; that fill an item before the store that publishes it, so a BAD-SHAPE
;;; here is not the publication race of the unlocked file.

(defpackage "GC-STRESS" (:use "CL"))
(in-package "GC-STRESS")

(defvar *publish-lock* (ccl:make-lock "publish"))

(load (merge-pathnames "gc-stress-threads.lisp" *load-truename*))
