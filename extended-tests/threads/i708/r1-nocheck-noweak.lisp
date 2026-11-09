;;; r1-nocheck-noweak.lisp -- r1-weak-tables.lisp with flags ("nocheck" "noweak"), for fork CI.
(in-package :cl-user)
(defvar *r1-flags* (list "nocheck" "noweak"))
(load (merge-pathnames "r1-weak-tables.lisp" *load-truename*))
