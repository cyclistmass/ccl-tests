;;; r1-nocheck-nokey.lisp -- r1-weak-tables.lisp with flags ("nocheck" "nokey"), for fork CI.
(in-package :cl-user)
(defvar *r1-flags* (list "nocheck" "nokey"))
(load (merge-pathnames "r1-weak-tables.lisp" *load-truename*))
