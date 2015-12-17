;;; notes and inspirations from reading On Lisp
;;; section of list utilities

;; the number of conses in a list is representable by this data type
;; this provides a easy foundation of fast list manipulations
;; because arithmetic ops on fixnums are fast
(deftype index (&optional (store 'list))
  (case store
    ('list 'fixnum)))

;; is last?
(declaim (inline last?))
(defun last? (x)
  (atom (cdr x)))

(defun ensure-list (x)
  "Ensure X is a list."
  (if (listp x) x (list x)))

(defun list2 (&rest args)
  "If (listp arg) then concat, otherwise add as a list element."
  (mappend #'ensure-list args))

(defun list-length1 (x)
  "Deals dot list. The second value is t if X is dotted"
   (do ((n 0 (+ n 2))
        (fast x (cddr fast))
        (slow x (cdr slow)))
       (nil)
     (when (null fast) (return n))
     (when (atom fast) (return (values (1+ n) t)))
     (when (null (cdr fast)) (return (1+ n)))
     (when (atom (cdr fast)) (return (values (+ n 2) t)))
     (when (and (eq fast slow) (> n 0)) (return nil))))

(defun proper-list? (x)
  "Is X a proper list?"
  (error "unimplemented"))

;; a utility find-lst that returns both the value from predicate and the list element
(defun find-list (predicate lst)
  (labels ((rec (predicate lst)
             (if (null lst)
                 nil
                 (let ((val (funcall predicate (car lst))))
                   (if val
                       (values (car lst) val)
                       (rec predicate (cdr lst))))))
           ;; iteration based
           (iter-find-lst (predicate lst)
             (block nil
                 (let ((fast lst)
                       (rest lst))
                   (loop
                      (if (null rest)
                          (return nil)
                          (let ((val (funcall predicate (car rest))))
                            (if val
                                (return (values (car rest) val))
                                (setf rest (cdr rest)))))
                      (setf fast (cddr fast))
                      (when (and fast
                                 (eq fast rest))
                        (error "circular list"))))))
           ;; or a more concise do recipe
           (iter-do-find-lst (predicate lst)
             (do ((fast (cddr lst) (cddr fast))
                  (rest lst (cdr rest)))
                 ((null rest) nil)
               (let ((val (funcall predicate (car rest))))
                 (when val
                   (return (values (car rest) val))))
               (when (and fast
                          (eq fast rest))
                 (error "circular list")))))
    (rec predicate lst)))


(defun my-reverse (lst)
  ;; they are all considerably slower than reverse
  (labels ((rev (lst)
             (if (null lst)
                 nil
                 (cons (rev (cdr lst)) (car lst))))
           ;; fastest
           (rev-tail (lst acc)
             (if (null lst)
                 acc
                 (rev-tail (cdr lst) (cons (car lst) acc))))
           ;; pretty slow
           (rev-reduce (lst)
             (reduce #'(lambda (x y)
                         (cons y x))
                     lst
                     :initial-value '()))
           ;; map, not as fast as tail version
           (rev-map (lst)
             (nreverse (mapcan #'(lambda (x) (list x)) lst))))
    (rev-tail lst nil)))


(defun my-copy-tree (tree)
  (labels ((rec (lst acc)
             (cond
               ;; null
               ((null lst) (nreverse acc))
               ;; nested
               ((consp (car lst))
                (rec (cdr lst) (cons (rec (car lst) nil) acc)))
               ;; atom
               (t (rec (cdr lst) (cons (car lst) acc))))))
    (rec tree nil)))


(defun group-lst (source n)
  "Group list SOURCE into N-length sublists possibly except the last.
e.g. (GROUP-LST '(1 2 3 4 5) 2) => ((1 2) (3 4) (5))"
  (labels ((rev (source acc)
             (let ((rest (nthcdr n source)))
               (if (consp rest)
                   (rev rest (cons (subseq source 0 n) acc))
                   (nreverse (cons source acc)))))
           ;; slightly slower
           (mapl-based (source n)
             (let ((result nil)
                   (counter n)
                   (temp nil))
               (mapc #'(lambda (c)
                         (setf temp (nconc temp (list c)))
                         (decf counter)
                         (when (zerop counter)
                           (progn (push temp result)
                                  (setf temp nil)
                                  (setf counter n))))
                     source)
               (when (consp temp)
                 (push temp result))
               (nreverse result))))
    (mapl-based source n)))


(defun flatten (lst)
  "Flatten a nested list."
  (labels ((rec-tail (lst acc)
             (cond ((null lst) (nreverse acc))
                   ((consp (car lst))
                    (rec-tail (cdr lst)
                              (nconc acc (rec-tail (car lst) nil))))
                   ;; (car lst) must be an atom (or a nil, but nil is atom too)
                   (t (rec-tail (cdr lst)
                                (cons (car lst) acc)))))
           (rec-tail-2 (lst acc)
             (cond ((null lst) acc)
                   ((consp (car lst)) (rec-tail (cdr lst)
                                                (rec-tail (car lst) acc)))
                   ;; (car lst) must be an atom (or a nil, but nil is atom too)
                   (t (rec-tail (cdr lst)
                                (cons (car lst) acc)))))
           (rec (x)
             (cond ((null x) nil)
                   ((atom x) (list x))
                   (t (nconc (rec (car x))
                             (rec (cdr x))))))
           (rec-backward (x acc)
             (cond ((null x) acc)
                   ((atom x) (cons x acc))
                   (t (rec-backward (car x) (rec-backward (cdr x) acc))))))
    ;; (nreverse (rec-tail-2 lst nil))))
    (rec-backward lst nil)))
    ;; (rec-tail lst nil)))

(defun prune-if (predicate lst)
  "Prune any leaves in LST which cause PREDICATE to be true."
  (labels ((rec-tail (x acc)
             (cond ((null x) (nreverse acc))
                   ((consp (car x))
                    (rec-tail (cdr x)
                              (cons (rec-tail (car x) nil) acc)))
                   ;; (car x) must be an atom
                   (t (rec-tail (cdr x)
                                (if (funcall predicate (car x))
                                    acc
                                    (cons (car x) acc)))))))
    (rec-tail lst nil)))


(defun before (x y lst &key (test #'eql))
  "Whether X occurs before Y in LST. Returns the cons that has the first X."
  (and lst
       (cond ((funcall test (car lst) x) lst)
             ((funcall test (car lst) y) nil)
             (t (before x y (cdr lst) :test test)))))

(defun after (x y lst &key (test #'eql))
  "Wether X occurs after Y."
  (let ((cons (before y x lst :test test)))
    (and cons (member x cons :test test))))

(defun nth-occurrence (n x lst &key (test #'eql))
  "Find the nth occurrence of X in LST. Return the containing cons and t.
If not found, then return the last cons and nil"
  (labels ((rec (n x lst mem)
             (if (<= n 0)
                 (values mem t)
                 (if (null lst)
                     (values mem nil)
                     (if (funcall test x (car lst))
                         (rec (1- n) x (cdr lst) lst)
                         (rec n x (cdr lst) lst))))))
    (rec n x lst lst)))

;; an example of 
(defun most (fn lst &key (test #'>))
  "The 'most-test-fn' element"
  (labels ((iter (fn lst)
             (if (null lst)
                 (values nil nil)
                 (let* ((record (car lst))
                        (record-val (funcall fn record)))
                   (dolist (x (cdr lst))
                     (let ((val (funcall fn x)))
                       (when (funcall test val record-val)
                         (setf record x
                               record-val val))))
                   (values record record-val)))))
    (iter fn lst)))



;; (dolists (((x lst1) (y lst2) ... ) [result-form]) . body)
(defmacro dolists ((binds &optional result-form) &body body)
  "Multiple DOLIST"
  (flet ((valid-bind? (bind)
           (and (consp bind)
                (= 2 (length bind))   ; must be pair
                (symbolp (car bind))))) ; name must be literal
    `(do ,(mapcar #'(lambda (bind)
                      (if (valid-bind? bind)
                          (let ((var-name (car bind)))
                            `(,var-name ,(cadr bind) (cdr ,var-name)))))
                  binds)
         ((not (and ,@(mapcan #'(lambda (bind)
                                 (if (valid-bind? bind)
                                     (list (car bind))))
                             binds)))
          ,result-form)
       ,@body)))

;; (dolists1 (((x lst1) (y lst2) ... ) [result-form]) . body)
(defmacro dolists1 ((binds &optional result-form) &body body)
  "Multiple DOLIST. End when all lists run out."
  (flet ((valid-bind? (bind)
           (and (consp bind)
                (= 2 (length bind))   ; must be pair
                (symbolp (car bind))))) ; name must be literal
    `(do ,(mapcar #'(lambda (bind)
                      (if (valid-bind? bind)
                          (let ((var-name (car bind)))
                            `(,var-name ,(cadr bind) (cdr ,var-name)))))
                  binds)
         ((not (or ,@(mapcan #'(lambda (bind)
                                 (if (valid-bind? bind)
                                     (list (car bind))))
                             binds)))
          ,result-form)
       ,@body)))








(defun transpose (lsts-of-lsts)
  "((1) (2 3)) => (1 2) (nil 3)"
  (error "Not implemented"))


;; nreverse does not guarantee this behavior but only imply that ANY modification
;; might occur to the original list.
(defun lnreverse (list)
  "Reversing LIST by pointing cdr to the previous cons."
  (cond ((atom list) nil)
        ((consp list)
         (do ((r-next (cdr list) (cdr r-next))
              (r list r-next)
              (head nil r))
             ((or (null r-next) (and (eq list r) head))
              (setf (cdr r) head)
              (values r list))
           (setf (cdr r) head)))
        (t
         (error "Invalid type (not a consp or atom) for lnreverse"))))

;; ;; tail recursive version
;; (defun lnreverse (list)
;;   "Reversing LIST by pointing cdr to the previous cons."
;;   (labels ((rec (list acc)
;;              (cond ((atom list) acc)
;;                    (t
;;                     (rec (cdr list)
;;                          (progn (setf (cdr list) acc)
;;                                 list))))))
;;     (if (atom list)
;;         nil
;;         (values (rec list nil) list))))

(defun nreverse-nconc (first second)
  "NREVERSE the FIRST and nconc it with SECOND"
  (multiple-value-bind (head tail) (lnreverse first)
    (setf (cdr tail) second)
    head))


(defun shuffle (x y)
  (macrolet ((rev-append (first second)
               `(multiple-value-bind (head tail) (lnreverse ,first)
                  (setf (cdr tail) ,second)
                  head)))
    (labels ((rec (x y acc)
               (cond ((null x) (rev-append acc y))
                     ((null y) (rev-append acc y))
                     (t
                      (rec (cdr x) (cdr y) (list* (car y) (car x) acc))))))
      (cond ((null x) y)
            ((null y) x)
            (t (rec x y nil))))))

;; this version denotes subseq as [head, tail) and uses head as pivot
(defun list-nqsort (list &optional (fn #'<))
  (labels ((rec (head tail len fn)
             (declare (type index len)
                      (type function fn))
             (cond ((< len 2) nil)
                   ((= len 2)
                    (let ((tail (cdr head)))
                      (unless (funcall fn (car head) (car tail))
                        (rotatef (car head) (car tail)))))
                   ((= len 3)
                    (let ((2nd (cdr head))
                          (3rd (cddr head)))
                      (unless (funcall fn (car head) (car 2nd))
                                       (rotatef (car head) (car 2nd)))
                      (unless (funcall fn (car head) (car 3rd))
                        (rotatef (car head) (car 3rd)))
                      (unless (funcall fn (car 2nd) (car 3rd))
                        (rotatef (car 2nd) (car 3rd)))))
                   (t
                    (let* ((first 0)
                           ;; pivot is first item
                           (pivot (car head))
                           (r-part
                             ;; ... -> r-part ->| second half
                             ;;     first half  |
                             (do ((r1 head)
                                  (r2 (cdr head) (cdr r2)))
                                 ((eq r2 tail) r1)
                               (when (funcall fn (car r2) pivot)
                                 (cdrf r1)
                                 (when (not (eq r2 r1))
                                   (rotatef (car r2) (car r1)))
                                 (incf first)))))
                      (declare (type index first))
                      (rotatef (car r-part) (car head))
                      (rec head r-part first fn)
                      (rec (cdr r-part) tail (- len first 1) fn))))))
    (if (null list)
        nil
        (rec list (cdr (last list)) (length list) fn))))

;; (defun q-sort (l &optional (f #'<))
;;   (if (null (cdr l)) l
;;     (append (q-sort (remove-if-not #'(lambda (x) (funcall f x (car l))) (cdr l)) f)
;;             (list (car l))
;;             (q-sort (remove-if #'(lambda (x) (funcall f x (car l))) (cdr l)) f))))

(defun gen-rand-flat-list (n)
  (do ((ret nil (push (random 100) ret))
       (i n (decf i)))
      ((zerop i) ret)))

(defun test-list-nqsort (rep &optional (n 100))
  (labels ((non-decreasing? (list)
             (cond ((null list) t)
                   (t (let ((next (cdr list)))
                        (or (null next)
                            (and (<= (car list) (car next))
                                 (non-decreasing? next))))))))
    (let ((failures '()))
      (dotimes (i rep)
        (let* ((list (gen-rand-flat-list n))
               (list-sorted (list-nqsort (copy-list list))))
          (if (not (non-decreasing? list-sorted))
              (push (list list list-sorted) failures))))
      failures)))
