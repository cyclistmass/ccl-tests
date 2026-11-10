;;; r1-diag.lisp -- at the first INVALID-MEMORY-ACCESS in an R1 worker,
;;; check every key and value of every R1 table by its raw words.
;;; With the other threads suspended and the GC locked, the words are
;;; copied out as raw bits, so no bad pointer is ever held as a Lisp
;;; object.  An EQUAL key must be a cons of a fixnum and NIL.  Every
;;; other key and every value must be an ITEM, so its header word must
;;; be the header word of a fresh ITEM.
(in-package :cl-user)
(defvar *diag-done* nil)
(defun diag-word () (if (> most-positive-fixnum (expt 2 32)) 8 4))
(defun diag-areas ()
  (let ((sh (if (= (diag-word) 8) 3 2)) (out nil))
    (ccl::do-consing-areas (a)
      (push (cons (ash (ccl::%fixnum-ref a target::area.low) sh)
                  (ash (ccl::%fixnum-ref a target::area.high) sh))
            out))
    out))
(defun diag-mapped-p (x areas)
  (find-if (lambda (a) (and (<= (car a) x) (< x (cdr a)))) areas))
(defun diag-copy (v)
  ;; Return (values raw-slot-words data-address) for the gvector V.
  (let* ((w (diag-word)) (n (ccl::uvsize v))
         (dest (make-array n :element-type (if (= w 8) '(unsigned-byte 64) '(unsigned-byte 32)))))
    (ccl::with-macptrs ((p))
      (ccl::%vect-data-to-macptr v p)
      (ccl::%copy-ptr-to-ivector p 0 dest 0 (* n w))
      (values dest (ccl::%ptr-to-int p)))))
(defun diag-dnode (x)
  ;; The two raw words of the dnode that holds tagged pointer X.
  (let* ((w (diag-word)) (base (logandc2 x (1- (* 2 w))))
         (dest (make-array 2 :element-type (if (= w 8) '(unsigned-byte 64) '(unsigned-byte 32)))))
    (ccl::with-macptrs ((p))
      (ccl::%vect-data-to-macptr dest p)
      (ccl::%copy-ptr-to-ivector p (- base (ccl::%ptr-to-int p)) dest 0 (* 2 w)))
    dest))
(defun diag-table (h areas item-header nil-word markers)
  (multiple-value-bind (raw addr) (diag-copy (ccl::nhash.vector h))
    (let ((equalp (eq (hash-table-test h) 'equal)) (fix (1- (diag-word)))
          (bad nil) (nbad 0) (nkeys 0))
      (flet ((check (word as-cons)
               ;; NIL if WORD is good, else a short description.
               (cond ((member word markers) nil)
                     ((zerop (logand word fix)) (and as-cons :fixnum))
                     ((not (diag-mapped-p word areas)) :unmapped)
                     (t (let* ((d (diag-dnode word)) (a (aref d 0)) (b (aref d 1)))
                          (if as-cons
                              (unless (or (and (= a nil-word) (zerop (logand b fix)))
                                          (and (= b nil-word) (zerop (logand a fix))))
                                (list :dnode a b))
                              (unless (= a item-header) (list :header a))))))))
        (loop for i from 14 below (length raw) by 2
              for kw = (aref raw i) for vw = (aref raw (1+ i))
              for kbad = (check kw equalp)
              for vbad = (check vw nil)
              do (unless (member kw markers) (incf nkeys))
                 (when (or kbad vbad)
                   (incf nbad)
                   (when (< (length bad) 12) (push (list i kw kbad vw vbad) bad)))))
      (list (format nil "DIAG table ~s weak ~s vec-data #x~x slots ~d flags-word #x~x keys ~d bad-pairs ~d"
                    (hash-table-test h) (ccl:hash-table-weak-p h) addr (length raw) (aref raw 1) nkeys nbad)
            (nreverse bad)))))
(defun diag-on-fault (c h k)
  (unless *diag-done*
    (unless (eq c :selftest) (setq *diag-done* t))
    (let* ((item (make-item :tag 0)) (out nil) header)
      (ccl::with-other-threads-suspended
        (ccl::without-gcing
          (let* ((areas (diag-areas))
                 (nil-word (ccl::%address-of nil))
                 (markers (list (ccl::%address-of (ccl::%unbound-marker))
                                (ccl::%address-of (ccl::%slot-unbound-marker)))))
            (setq header (aref (diag-dnode (ccl::%address-of item)) 0))
            (setq out (list areas nil-word markers
                            (mapcar (lambda (tb) (diag-table tb areas header nil-word markers)) *tables*))))))
      (destructuring-bind (areas nil-word markers tables) out
        (format t "~&DIAG fault ~a in table ~s, new key ~s, gcs ~s~%" c (hash-table-test h) k (ccl:gccounts))
        (format t "~&DIAG nil #x~x markers ~{#x~x ~}item-header #x~x~%" nil-word markers header)
        (dolist (a areas) (format t "~&DIAG area #x~x-#x~x~%" (car a) (cdr a)))
        (dolist (tb tables)
          (format t "~&~a~%" (first tb))
          (dolist (b (second tb))
            (format t "~&DIAG   slot ~d key #x~x ~s value #x~x ~s~%"
                    (first b) (second b) (third b) (fourth b) (fifth b)))))
      (finish-output))))
(setq *on-fault* 'diag-on-fault)
