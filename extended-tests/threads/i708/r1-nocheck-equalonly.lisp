;;; r1-nocheck-equalonly.lisp -- r1-weak-tables.lisp with flags ("nocheck" "equalonly"), for fork CI.
(in-package :cl-user)
(defvar *r1-flags* (list "nocheck" "equalonly"))
(load (merge-pathnames "r1-weak-tables.lisp" *load-truename*))
