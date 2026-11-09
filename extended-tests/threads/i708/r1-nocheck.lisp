;;; r1-nocheck.lisp -- r1-weak-tables.lisp with flags ("nocheck"), for fork CI.
(in-package :cl-user)
(defvar *r1-flags* (list "nocheck"))
(load (merge-pathnames "r1-weak-tables.lisp" *load-truename*))
