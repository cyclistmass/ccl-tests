;;; r1-nocheck-catch.lisp -- r1-weak-tables.lisp with flags ("nocheck") and the EQUAL-key catch, for fork CI.
(in-package :cl-user)
(defvar *r1-flags* (list "nocheck"))
(load (merge-pathnames "r1-catch.lisp" *load-truename*))
(load (merge-pathnames "r1-weak-tables.lisp" *load-truename*))
