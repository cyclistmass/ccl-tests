;;; r1-weak-tables.lisp
(in-package :cl-user)
(defvar *duration* 90)
(defvar *stop* nil)
(defvar *bad* nil)
;; CCL: "Only EQ hash tables can be weak" unless :weak :value
;; (read: level-0/l0-hash.lisp 489-500), so weak-on-key tables are EQ only.
(defvar *tables*
  (list (make-hash-table :test 'eq    :weak :key   :shared t)
        (make-hash-table :test 'eq    :weak :value :shared t)
        (make-hash-table :test 'equal :weak :value :shared t)   ; EQUAL derefs keys on rehash
        (make-hash-table :test 'eql   :weak :value :shared t)))
(defstruct item tag (payload (make-array 5 :initial-element 0)))
;; An item whose payload holds its tag, so ITEM-OK-P can check it.
(defun mk (tag) (let ((i (make-item :tag tag))) (setf (svref (item-payload i) 0) tag) i))
(defun item-ok-p (x) (and (item-p x) (simple-vector-p (item-payload x))
                          (eql (svref (item-payload x) 0) (item-tag x))))
(defun worker (seed)
  (let ((rs (make-random-state t)) (n 0))
    (loop until *stop* do
      (dolist (h *tables*)
        (let* ((tag (+ seed (* 8 (random 100000 rs))))
               (k (if (eq (hash-table-test h) 'equal) (list tag) (make-item :tag tag)))
               (v (let ((i (make-item :tag tag))) (setf (svref (item-payload i) 0) tag) i)))
          (setf (gethash k h) v)                 ; key or value dies at once: weak slot churn
          (when (zerop (mod n 7)) (remhash k h))
          (when (zerop (mod n 1000))              ; force rehash + key-moved handling
            (maphash (lambda (k v) (declare (ignore k))
                       (unless (item-ok-p v) (setq *bad* (format nil "table ~a" h)) (setq *stop* t)))
                     h))
          (incf n))))))
(defun run ()
  (setq ccl::*gc-event-status-bits*
        (logior ccl::*gc-event-status-bits* (ash 1 2)))
  (dotimes (i 1000) (dolist (h *tables*) (setf (gethash (make-item :tag i) h) (mk i))))
  (ccl:gc)                                        ; tenure the four hash vectors
  (let ((procs (loop for i below 8 collect
                 (ccl:process-run-function (format nil "w~d" i) (lambda () (worker i))))))
    (loop repeat *duration* until *stop* do (sleep 1))
    (setq *stop* t) (mapc #'ccl:join-process procs)
    (format t "~&R1 :VERDICT ~a :GCS ~a~@[ :WHAT ~a~]~%" (if *bad* "BAD-SHAPE" "PASS") (ccl:gccounts) *bad*)
    (ccl:quit (if *bad* 1 0))))
(run)
