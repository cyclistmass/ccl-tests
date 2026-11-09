;;; r3-wide-check.lisp -- gc-stress-threads-wide.lisp for 120 s with the
;;; kernel's GC integrity check on: every GC first checks the header of
;;; every object that any area points to, and stops with the address of
;;; the first bad one.

(setq ccl::*gc-event-status-bits*
      (logior ccl::*gc-event-status-bits* (ash 1 2)))

(defpackage "GC-STRESS" (:use "CL"))
(in-package "GC-STRESS")

(defvar *duration* 120)

(load (merge-pathnames "../gc-stress-threads-wide.lisp" *load-truename*))
