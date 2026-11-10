;;; r1-weak-tables.lisp
(in-package :cl-user)
(defvar *duration* 90)
(defvar *stop* nil)
(defvar *bad* nil)
;; CCL: "Only EQ hash tables can be weak" unless :weak :value
;; (read: level-0/l0-hash.lisp 489-500), so weak-on-key tables are EQ only.
;; Flags come from the command line, or from a wrapper that sets *R1-FLAGS*
;; first (fork CI passes no arguments).
(defvar *r1-flags* ccl:*command-line-argument-list*)
;; A wrapper can set *ON-FAULT* to a function of (condition table key),
;; called before the fault unwinds.
(defvar *on-fault* nil)
(defun flag (f) (member f *r1-flags* :test #'equal))
(defvar *weak* (not (flag "noweak")))
(defun weak-arg (w) (and *weak* w))
;; "noweak" makes the same four tables with no weakness: the control that
;; says whether weakness is the variable.
;; "locked" makes the tables shared but not lock-free, to tell the
;; lock-free algorithm from the weak processing.
(defvar *lock-free* (if (flag "locked") nil :shared))
(defvar *tables*
  ;; "bigkey" sizes the weak-on-key table so it never grows: its vector is
  ;; made once, before the churn, so issue #708 has no window on it.
  (list (apply #'make-hash-table :test 'eq :weak (weak-arg :key) :shared t :lock-free *lock-free*
               (and (flag "bigkey") (list :size 400000)))
        (make-hash-table :test 'eq    :weak (weak-arg :value) :shared t :lock-free *lock-free*)
        (make-hash-table :test 'equal :weak (weak-arg :value) :shared t :lock-free *lock-free*)
        (make-hash-table :test 'eql   :weak (weak-arg :value) :shared t :lock-free *lock-free*)))
;; "keyonly" keeps only the weak-on-key EQ table.
(when (flag "keyonly") (setq *tables* (list (first *tables*))))
;; "equalonly" keeps only the weak-on-value EQUAL table.
(when (flag "equalonly") (setq *tables* (list (third *tables*))))
;; "nokey" drops the weak-on-key EQ table and keeps the three weak-on-value ones.
(when (flag "nokey") (setq *tables* (rest *tables*)))
;; The EQUAL tables, found once: a wrapper may replace their compare
;; function, and then HASH-TABLE-TEST no longer names EQUAL.
(defvar *equal-tables* (remove-if-not (lambda (h) (eq (hash-table-test h) 'equal)) *tables*))
;; A wrapper can set *ON-TABLES* to a function of the table list, called
;; once before the tables are filled.
(defvar *on-tables* nil)
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
               (k (if (member h *equal-tables*) (list tag) (make-item :tag tag)))
               (v (let ((i (make-item :tag tag))) (setf (svref (item-payload i) 0) tag) i)))
          (handler-bind ((ccl::invalid-memory-access
                           (lambda (c) (when *on-fault* (funcall *on-fault* c h k)))))
            (setf (gethash k h) v))              ; key or value dies at once: weak slot churn
          (when (zerop (mod n 7)) (remhash k h))
          (when (zerop (mod n 1000))              ; force rehash + key-moved handling
            (maphash (lambda (k v) (declare (ignore k))
                       (unless (item-ok-p v) (setq *bad* (format nil "table ~a weak ~s value ~s" h (ignore-errors (ccl:hash-table-weak-p h)) (ignore-errors (prin1-to-string v)))) (setq *stop* t)))
                     h))
          (incf n))))))
(defun run ()
  ;; "nocheck" leaves the kernel's GC integrity check off, to tell a check
  ;; that fires on a legitimate weak forward from real damage.
  (unless (flag "nocheck")
    (setq ccl::*gc-event-status-bits*
          (logior ccl::*gc-event-status-bits* (ash 1 2))))
  ;; "noegc" turns the ephemeral GC off, so every GC is a full one.
  (when (flag "noegc") (ccl:egc nil))
  (format t "~&R1 check ~s weak ~s tables ~d egc ~s lock-free ~s~%" (logbitp 2 ccl::*gc-event-status-bits*) *weak*
          (length *tables*) (ccl:egc-enabled-p) (ccl::hash-lock-free-p (first *tables*)))
  (finish-output)
  (when *on-tables* (funcall *on-tables* *tables*))
  (dotimes (i 1000) (dolist (h *tables*) (setf (gethash (make-item :tag i) h) (mk i))))
  (ccl:gc)                                        ; tenure the four hash vectors
  ;; "diagnow" runs the fault hook once here, as its positive control.
  (when (and *on-fault* (flag "diagnow")) (funcall *on-fault* :selftest (first *tables*) nil))
  (let ((procs (loop for i below (if (flag "one") 1 8) collect
                 (ccl:process-run-function (format nil "w~d" i) (lambda () (worker i))))))
    (loop repeat *duration* until *stop* do (sleep 1))
    (setq *stop* t) (mapc #'ccl:join-process procs)
    (format t "~&R1 :VERDICT ~a :GCS ~a :KEY-SLOTS ~d~@[ :WHAT ~a~]~%" (if *bad* "BAD-SHAPE" "PASS") (ccl:gccounts)
            (ccl::uvsize (ccl::nhash.vector (first *tables*))) *bad*)
    (ccl:quit (if *bad* 1 0))))
(run)
