;; mapcar, etc.

(defun map-> (fn start end-fn succ-fn)
  "Apply FN to sequence that starts from START, extends with SUCC-FN, and ends
by END-FN."
  (do ((x start (funcall succ-fn x))
       (result nil))
      ((funcall end-fn x) (nreverse result))
    (push (funcall fn x) result)))

(defun mapa->b (fn start end &optional (step 1))
  (cond
    ;; inc
    ((and (> step 0) (>= end start))
     (map-> fn start #'(lambda (x) (> x end)) #'(lambda (x) (+ x step))))
    ;; dec
    ((and (> step 0) (>= end start))
     (map-> fn start #'(lambda (x) (< x end)) #'(lambda (x) (+ x step))))
    ;; infinite seq
    (t (error "Infinite sequence"))))

(defun map0->n (fn n)
  (mapa->b fn 0 n))

(defun map1->n (fn n)
  (mapa->b fn 1 n))



;;; map with index

(declaim (inline fast-nthcdr))
(defun fast-nthcdr (n list)
  (declare (type index n))
  (do ((i n (1- i))
       (result list (cdr result)))
      ((not (plusp i)) result)
    (declare (type index i))))

(declaim (inline cdrw))
(defun cdrw (list head)
  (macrolet ((strict-cdrw (l h)
               (once-only (l h)
                 `(let ((,l (cdr ,l)))
                    (if (atom ,l) ,h ,l)))))
    (cond ((null list) (strict-cdrw head head))
          (t (strict-cdrw list head)))))



;; iterate with index and steps
;; inspired by map1 in SBCL
(defun list-map (fn arglists step acc-scheme take-car)
  "Iterate through lists with step in a map style"
  (flet ((iter (fn arglists step acc-scheme take-car)
           (declare (type index step))
           (let* ((ret (list nil))
                  (non-acc-ret (car arglists))
                  (acc ret)
                  (index 0)
                  (args (cons 0 (make-list (length arglists))))
                  (result nil))
             (declare (type index index))
             (unless (dolist (x arglists) (or x (return t)))
               (do () (nil)
                 ;; set up args
                 (do ((l arglists (cdr l))
                      (arg (cdr args) (cdr arg)))
                     ((null l))
                   (setf (car arg) (if take-car (caar l) (car l)))
                   ;; move to the next position in all lists
                   (setf (car l) (fast-nthcdr step (car l)))) ; step is of type
                                                              ; index
                 ;; apply fn
                 (setf result (apply fn args))
                 (case acc-scheme
                   (:nconc
                    (setf (cdr acc) result)
                    (when (consp result)
                      (setf acc (last result))))
                   (:list
                    (setf (cdr acc) (list result)
                          acc (cdr acc)))
                   (:append
                    (if (consp result)
                        (setf result (copy-list result)
                              (cdr acc) result
                              acc (last result))
                        (setf (cdr acc) result))))
                 ;; finish loop
                 (when (dolist (x arglists) (or x (return t))) (return))
                 ;; increment index
                 ;; we need to do this after checking loop finishing
                 ;; because this way index + step is surely of type index
                 (incf index step)
                 (setf (car args) index)))
             ;; return
             (if acc-scheme
                 (cdr ret)
                 non-acc-ret))))

    (when (< step 1) (error "STEP must be larger than zero."))
    (typecase step
      (index (iter fn arglists step acc-scheme take-car))
      (t (unless (dolist (x arglists) (or x (return t)))
           (let* ((args (cons 0 (do ((l arglists (cdr l))
                                     (args))
                                    ((null l) (nreverse args))
                                  (push (if take-car (caar l) (car l)) args))))
                  (result (apply fn args)))
             (case acc-scheme
               ((:nconc :append) result)
               (:list (list result)))))))))



(defun list-map-wrap (fn arglists step acc-scheme take-car)
  "Iterate through lists with step in a map style and wraps around. The last
list exhausted does not wraps around. E.g. (1 2 3 4 5) with step 3 yields 1 4,
not 1 4 2."
  (when (< step 1) (error "STEP must be larger than zero."))
  (do* ((ret (list nil))
        (non-acc-ret (car arglists))
        (acc ret)
        (index 0)
        (args (cons 0 (do ((l arglists (cdr l))
                           (res))
                          ((null l) (nreverse res))
                        (push (if take-car (caar l) (car l)) res))))
        ;; nil: not first reached yet
        (finish-flags (make-list (length arglists)))
        (anchors (prog1 arglists
                   (setf arglists (copy-list arglists))))
        (result nil))
       (;; the loop stops when all are T
        (not (dolist (x finish-flags) (or x (return t))))
        (if acc-scheme
            (cdr ret)
            non-acc-ret))
    ;; apply fn
    (setf result (apply fn args))
    (case acc-scheme
      (:nconc
       (setf (cdr acc) result)
       (when (consp result)
         (setf acc (last result))))
      (:list
       (setf (cdr acc) (list result)
             acc (cdr acc)))
      (:append
       (if (consp result)
           (setf result (copy-list result)
                 (cdr acc) result
                 acc (last result))
           (setf (cdr acc) result))))
    ;; increment index
    (incf index step)
    (setf (car args) index)
    ;; move to next
    (do ((l arglists (cdr l))
         (arg (cdr args) (cdr arg))
         (flag finish-flags (cdr flag))
         (anchor anchors (cdr anchor)))
        ((null l))
      (multiple-value-bind (next wrapped)
          (nthcdr-wrap step (car l) (car anchor))
        (setf (car l) next
              (car arg) (if take-car (car next) next))
        (and wrapped
             (null (car flag))
             (setf (car flag) t))))))



(defun to-end (l acc)
  "How long is it to the end of the list? How much cdr calls will first reach
nil?"
  (cond ((null l) acc)
        ((to-end (cdr l) (1+ acc)))))

(defun to-front (l head acc)
  (cond ((or (null head) (eq l head)) acc)
        (t (to-front l (cdr head) (1+ acc)))))


;; an enhancement of nthcdr
(defun nthcdr-wrap (n list &optional head)
  "Performs the cdr function n times on a list and wraps around on atom
tail. The atom on the tail is ignored."
  (declare (type index n))
  (labels ((cdrw (l h)
             (let ((c (cdr l)))
               (if (atom c) h c)))

           (move (n list head)
             (do ((i 0 (1+ i))
                  (j 0)
                  (r-i list)
                  (r-2i list (cdrw (cdrw r-2i head) head)))
                 ((and (eq r-i r-2i)
                       (not (zerop i)))
                  (let ((rest (mod n i)))
                    (if (< rest j)
                        (nthcdr rest r-i)
                        (nthcdr (- rest j) head))))
               (declare (type index i)
                        (type index j))
               ;; we don't care the type of n here
               ;; if n is larger than type index, it fails anyway
               (when (= n i) (return r-i))
               (let ((r (cdrw r-i head)))
                 (when (eq r head) (setf j i))
                 (setf r-i r))))

         (fast-move (n list head)
           (declare (type index n))
           (do ((i n)
                (j 0)
                (wrapped nil)
                (r-i list))
               ((not (plusp i)) (values r-i wrapped))
             (declare (type index i)
                      (type index j))
             (decf i)
             (let ((r (cdrw r-i head)))
               (when (eq r head)
                 (setf j (- n i))
                 (setf wrapped t))
               (when (eq r list)
                 (return (values (let* ((len (- n i))
                                        (rest (mod n len)))
                                   (if (< rest j)
                                       (nthcdr rest r)
                                       (nthcdr (- rest j) head)))
                                 wrapped)))
               (setf r-i r)))))

    (declare (inline cdrw))

    (if head
        (typecase n
          (index (fast-move n list head))
          (t (move n list head)))
        (nthcdr n list))))



;; The reason why we avoid building mapcari upon mapcar is that in the lambda of
;; mapcar, we need to &rest collect all arguments and this is runtime consing

(defun mapcari (fn lst &rest more-lsts)
  "MAPCAR with index"
  (list-map fn (cons lst more-lsts) 1 :list t))

(defun mapcani (fn lst &rest more-lsts)
  "MAPCAN with index"
  (list-map fn (cons lst more-lsts) 1 :nconc t))

(defun mappendi (fn lst &rest more-lsts)
  (list-map fn (cons lst more-lsts) 1 :append t))

(defun maplisti (fn lst &rest more-lsts)
  (list-map fn (cons lst more-lsts) 1 :list nil))

(defun mapconi (fn lst &rest more-lsts)
  (list-map fn (cons lst more-lsts) 1 :nconc nil))

(defun mapci (fn lst &rest more-lsts)
  (list-map fn (cons lst more-lsts) 1 nil t))

(defun mapli (fn lst &rest more-lsts)
  (list-map fn (cons lst more-lsts) 1 nil nil))



(defun mappend (fn &rest lsts)
  "Nondestructive MAPCAN"
  (apply #'append (apply #'mapcar fn lsts)))

(defun rmapcar (fn &rest args)
  (if (some #'atom args)
      (apply fn args)
      (apply #'mapcar
             #'(lambda (&rest args)
                          (apply #'rmapcar fn args))
             args)))
