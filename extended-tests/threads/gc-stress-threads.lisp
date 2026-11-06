;;; gc-stress-threads.lisp -- many threads cons, stack-allocate and store
;;; young objects into old ones while the GC runs.  Upstream issue #708.
;;;
;;; #708 reports two crashes in the x86-64 GC, both from an allocation trap
;;; in a program with threads: one while the GC scans a temp stack, one while
;;; the EGC follows an old-to-young reference.  In both, the GC read a word
;;; as an object header that is not a header.  No test case came with it.
;;;
;;; This file runs three kinds of worker at once, for DURATION seconds:
;;;   - DE workers make dynamic-extent lists and vectors, and cons on the
;;;     heap while those are live, so a GC finds them on the stack.
;;;   - STORE workers put fresh conses and vectors into a vector and conses
;;;     that were made before a full GC, so the EGC must track them.
;;;   - CHECK workers read the stored objects back and verify their shape.
;;;
;;; Every allocated value is used: CCL removes an allocation whose value is
;;; discarded, and then no GC happens.
;;;
;;; Prints one line:  GC-STRESS-RESULT :VERDICT v :GCS n :STORES n :DE n
;;; and exits 0 only on PASS.  A heap error shows as a crash (a nonzero exit),
;;; or as :VERDICT BAD-SHAPE.  The caller must kill the run on a timeout.

(defpackage "GC-STRESS" (:use "CL"))
(in-package "GC-STRESS")

(defparameter *duration* 60)
(defparameter *de-workers* 3)
(defparameter *store-workers* 3)
(defparameter *check-workers* 2)
(defparameter *slots* 4096)

(defvar *stop* nil)
(defvar *bad* nil)
(defvar *old-vector*)
(defvar *old-conses*)
(defvar *table*)
(defvar *de-count* 0)
(defvar *store-count* 0)

;;; A stored object is (tag . payload): tag is a fixnum, payload a simple
;;; vector of length (mod tag 17) whose every element is tag.
(defun make-item (tag)
  (cons tag (make-array (mod tag 17) :initial-element tag)))

(defun item-ok-p (x)
  (or (null x)
      (and (consp x)
           (typep (car x) 'fixnum)
           (simple-vector-p (cdr x))
           (= (length (cdr x)) (mod (car x) 17))
           (every (lambda (e) (eql e (car x))) (cdr x)))))

(defun note-bad (what x)
  (unless *bad*
    (setq *bad* (format nil "~a ~s" what (ignore-errors (type-of x))))))

(defun sum-list (l)
  (let ((s 0)) (dolist (e l s) (incf s e))))

(defun de-step (n)
  (let ((v (make-array n :initial-element 1))
        (l (make-list n :initial-element 2)))
    (declare (dynamic-extent v l))
    ;; Heap consing while v and l are live on the stack.
    (let ((heap (loop repeat 8 collect (make-array 20 :initial-element n))))
      (+ (reduce #'+ v) (sum-list l) (length heap)
         (apply #'de-rest v)))))

(defun de-rest (&rest args)
  (declare (dynamic-extent args))
  (length args))

(defun de-worker ()
  (let ((acc 0) (n 0))
    (loop until *stop* do
      (incf acc (de-step (1+ (random 200))))
      (incf n))
    (atomic-incf-de n)
    acc))

(defun store-worker (seed)
  (let ((state (make-random-state t)) (n 0))
    (loop until *stop* do
      (let* ((tag (+ seed (* 8 (random 1000000 state))))
             (i (random *slots* state)))
        (setf (svref *old-vector* i) (make-item tag))
        (setf (car (svref *old-conses* i)) (make-item (1+ tag)))
        (when (zerop (mod n 64))
          (setf (gethash (mod tag 2048) *table*) (make-item tag)))
        (incf n)))
    (atomic-incf-store n)))

(defun check-worker ()
  (loop until *stop* do
    (dotimes (i *slots*)
      (let ((x (svref *old-vector* i)))
        (unless (item-ok-p x) (note-bad "vector-slot" x)))
      (let ((c (svref *old-conses* i)))
        (if (consp c)
            (unless (item-ok-p (car c)) (note-bad "old-cons-car" (car c)))
            (note-bad "old-cons" c))))
    (maphash (lambda (k v)
               (declare (ignore k))
               (unless (item-ok-p v) (note-bad "table" v)))
             *table*)))

(defvar *count-lock* (ccl:make-lock))
(defun atomic-incf-de (n) (ccl:with-lock-grabbed (*count-lock*) (incf *de-count* n)))
(defun atomic-incf-store (n) (ccl:with-lock-grabbed (*count-lock*) (incf *store-count* n)))

(defun gc-count ()
  ;; All GCs, full and ephemeral, from the GC's own counter.
  (values (ccl:gccounts)))

(defun run ()
  (setq *old-vector* (make-array *slots* :initial-element nil)
        *old-conses* (let ((v (make-array *slots*)))
                       (dotimes (i *slots* v) (setf (svref v i) (cons nil i))))
        *table* (make-hash-table :test 'eql :shared t))
  ;; Promote the containers out of the young generation, so each store of
  ;; a fresh object into them is an old-to-young reference.
  (ccl:gc)
  (let ((procs '()))
    (dotimes (i *de-workers*)
      (push (ccl:process-run-function (format nil "de-~d" i) #'de-worker) procs))
    (dotimes (i *store-workers*)
      (let ((seed i))
        (push (ccl:process-run-function (format nil "store-~d" i)
                                        (lambda () (store-worker seed)))
              procs)))
    (dotimes (i *check-workers*)
      (push (ccl:process-run-function (format nil "check-~d" i) #'check-worker) procs))
    (let ((end (+ (get-universal-time) *duration*)))
      (loop while (and (< (get-universal-time) end) (null *bad*))
            do (sleep 1)))
    (setq *stop* t)
    (dolist (p procs) (ccl:join-process p))
    (let ((verdict (if *bad* "BAD-SHAPE" "PASS")))
      (format t "~&GC-STRESS-RESULT :VERDICT ~a :GCS ~a :EGC ~a :STORES ~d :DE ~d~@[ :WHAT ~a~]~%"
              verdict (gc-count) (ccl:egc-enabled-p) *store-count* *de-count* *bad*)
      (finish-output)
      (ccl:quit (if *bad* 1 0)))))

(run)
