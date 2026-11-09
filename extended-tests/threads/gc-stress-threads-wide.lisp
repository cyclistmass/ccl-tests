;;; gc-stress-threads-wide.lisp -- gc-stress-threads.lisp, wider: 16 threads,
;;; an interrupt storm, dynamic-extent objects up to 2000 elements, and 10
;;; minutes.  Aimed at the two traces in issue #708 (a temp stack scan and
;;; an EGC old-to-young root, both reached from an allocation trap).
;;; Stores and reads go under the publish lock, so a weakly ordered machine
;;; does not report the publication race of the unlocked file.

(defpackage "GC-STRESS" (:use "CL"))
(in-package "GC-STRESS")

(defvar *duration* 600)
(defvar *de-workers* 6)
(defvar *store-workers* 5)
(defvar *check-workers* 3)
(defvar *interrupters* 2)
(defvar *de-max* 2000)
(defvar *publish-lock* (ccl:make-lock "publish"))

(load (merge-pathnames "gc-stress-threads.lisp" *load-truename*))
