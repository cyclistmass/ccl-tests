;;; r1-nocheck-diag.lisp -- r1-weak-tables.lisp with flags "nocheck" "diagnow" and the r1-diag.lisp fault dump, for fork CI.
(in-package :cl-user)
(defvar *r1-flags* (list "nocheck" "diagnow"))
(defvar *on-fault* nil)
(load (merge-pathnames "r1-diag.lisp" *load-truename*))
(load (merge-pathnames "r1-weak-tables.lisp" *load-truename*))
