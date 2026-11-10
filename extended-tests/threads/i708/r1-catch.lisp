;;; r1-catch.lisp -- catch the bad table key at the moment EQUAL gets it.
;;; Every EQUAL table gets a compare function that classifies the table
;;; key by its raw bits before it calls EQUAL.  An EQUAL key in R1 is a
;;; cons (the churn) or an ITEM (the first fill), at or above the lowest
;;; consing area, so any other bits (except the free marker) are a bad
;;; key.  On the first bad key it records the raw word, where that word
;;; sits in the table now, the GC count and the call chain, and stops
;;; the run with BAD-SHAPE instead of calling EQUAL on it.
(in-package :cl-user)
(load (merge-pathnames "r1-diag.lisp" *load-truename*))
(defvar *catch-lowest* nil)
(defvar *catch-calls* 0)
(defvar *catch-free* 0)
(defvar *catch-done* nil)
(defun catch-class (w)
  ;; :FREE for the free marker, :OK for a cons or uvector pointer at or
  ;; above the lowest consing area, else :BAD.
  (cond ((eql w (ccl::%address-of (ccl::%unbound-marker))) :free)
        ((and (member (logand w target::fulltagmask) (list target::fulltag-cons target::fulltag-misc))
              (>= w *catch-lowest*))
         :ok)
        (t :bad)))
(defun catch-report (a b w)
  (declare (ignore b))
  (unless *catch-done*
    (setq *catch-done* t)
    (let ((found nil) (gcs nil) (dnode nil))
      (ccl::with-other-threads-suspended
        (ccl::without-gcing
          (setq gcs (multiple-value-list (ccl:gccounts)))
          (when (diag-mapped-p w (diag-areas))
            (setq dnode (coerce (diag-dnode w) 'list)))
          (dolist (h *tables*)
            (let ((raw (diag-copy (ccl::nhash.vector h))))
              (loop for i from 0 below (length raw)
                    when (eql (aref raw i) w)
                      do (push (list (position h *tables*) i (aref raw (logxor i 1))) found))))))
      (format t "~&CATCH bad table key #x~x for new key ~s gcs ~s~%" w a gcs)
      (format t "~&CATCH dnode at key ~:[unmapped~;~:*~{#x~x ~}~]~%" dnode)
      (format t "~&CATCH key word now in (table slot pair-word): ~s~%" found)
      (format t "~&CATCH calls ~d free ~d~%" *catch-calls* *catch-free*)
      (ccl:print-call-history :count 12 :detailed-p nil)
      (finish-output)
      (setq *bad* (format nil "CATCH bad key #x~x" w) *stop* t))))
(defun catch-equal (a b)
  (incf *catch-calls*)
  (let* ((w (ccl::%address-of b)) (class (catch-class w)))
    (case class
      (:ok (equal a b))
      (:free (incf *catch-free*) nil)
      (t (catch-report a b w) nil))))
(defun catch-install (tables)
  (setq *catch-lowest* (reduce #'min (mapcar #'car (diag-areas))))
  ;; Positive control: a small cons-tagged word is :BAD, a real cons and
  ;; a real ITEM are :OK, the free marker is :FREE.
  (format t "~&CATCH selftest ~s ~s ~s ~s lowest #x~x~%"
          (catch-class (+ #x20 target::fulltag-cons))
          (catch-class (ccl::%address-of (list 1)))
          (catch-class (ccl::%address-of (make-item :tag 1)))
          (catch-class (ccl::%address-of (ccl::%unbound-marker)))
          *catch-lowest*)
  (dolist (h tables)
    (when (eq (hash-table-test h) 'equal)
      (setf (ccl::nhash.compareF h) #'catch-equal)
      (format t "~&CATCH installed on ~s~%" h)))
  (finish-output))
(defvar *on-tables* 'catch-install)
(push (lambda () (format t "~&CATCH summary calls ~d free ~d bad ~s~%" *catch-calls* *catch-free* *catch-done*)
        (finish-output))
      ccl::*lisp-cleanup-functions*)
