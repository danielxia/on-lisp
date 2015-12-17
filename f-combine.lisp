;;; functions definable by combining functions

;; comparable to non-composed version
(defun f. (&rest fns)
  "Compose FNS"
  (if fns
      (let* ((fns-rev (reverse fns))
             (init (car fns-rev))
             (pipeline (cdr fns-rev)))
        #'(lambda (&rest args)
            (reduce #'(lambda (x f) (funcall f x))
                    pipeline
                    :initial-value (apply init args))))
      #'identity))

(defun f-if (xif xthen &optional xelse)
  #'(lambda (&rest args)
      (if (apply xif args)
          (apply xthen args)
          (if xelse (apply xelse args)))))

;; slightly slower than raw lisp (but diff is very small)
(defun f-intersect (&rest fns)
  (labels ((rec (fns x result)
             (if (null fns)
                 result
                 (let ((result (apply (car fns) x)))
                   (and result
                        (rec (cdr fns) x result))))))
    #'(lambda (&rest args)
        (rec fns args t))))

(defun f-union (&rest fns)
  (labels ((rec (fns x result)
             (if (null fns)
                 result
                 (let ((result (apply (car fns) x)))
                   (or result
                        (rec (cdr fns) x result))))))
    #'(lambda (&rest args)
        (rec fns args t))))

;; function definable by recursing on cdrs
;; f(list) = g(car, f(cdr))
(defun f-lrec (g base)
  (labels ((rec (lst g base)
             (if (null lst)
                 (if (functionp base)
                     (funcall base)
                     base)
                 (funcall g (car lst) (rec (cdr lst) g base)))))
    #'(lambda (lst) (rec lst g base))))

;; textbook version
;; problem is that: a lambda is created at every step of recursion
(defun lrec (rec &optional base)
  (labels ((self (lst)
             (if (null lst)
                 (if (functionp base)
                     (funcall base)
                     base)
                 (funcall rec (car lst)
                          #'(lambda ()
                              (self (cdr lst)))))))
    #'self))


;; use macro to shortcut in some cases
;; e.g.
;; (funcall (f-lrec/m ((a fd) (and (> a 0) fd)) t) '(1 2 3 4 -1)) =>  nil
;; (funcall (f-lrec/m ((a fd c) (if (zerop a) (values c t) fd)) nil) '(1 2 0)) =>  (0) t
;;
;; Caveat: base is evaled first. So do not rely on side effects during recursion
;; :eager controls whether to symbol-macrolet or let
(defmacro f-lrec/macro (((a-sym fd-sym &optional (cons-sym nil suppliedp-cons))
                         g-form)
                        base-form &key eager)
  (with-gensyms (base list rec)
    `#'(lambda (,list)
         (let ((,base ,base-form))
           (labels ((,rec (,list) ;reuse the symbol ,list
                      (if (endp ,list)
                          (if (functionp ,base)
                              (funcall ,base)
                              ,base)
                          ,(list
                            (if eager
                                'let
                                'symbol-macrolet)
                            (list* `(,a-sym (car ,list))
                                   `(,fd-sym (,rec (cdr ,list)))
                                   (if suppliedp-cons
                                       (list `(,cons-sym ,list))
                                       nil))
                            g-form))))
             (,rec ,list))))))

;; tree traversal
;;
;; tree:
;; (non-cons) is tree
;; x, y are trees => (x . y) is a tree
;;
;; It only depends on the base value (leaf) and the combination of subtrees
;; f(tree) = g(fl, fr, l, r)
;; l: car, r: cdr
(defmacro f-trec/macro (((fl-sym fr-sym
                          &optional (l-sym nil sup-l) (r-sym nil sup-r))
                         g-form)
                        (leaf-form nil-form) &key eager)
  (with-gensyms (leaf nill tree rec)
    `#'(lambda (,tree)
         (let ((,leaf ,leaf-form)
               (,nill ,nil-form))
           (labels ((,rec (,tree)       ;reuse symbol ,tree
                      (cond ((consp ,tree)
                             ,(list
                               (if eager
                                   'let
                                   'symbol-macrolet)
                               (list* `(,fl-sym (,rec (car ,tree)))
                                      `(,fr-sym (if (cdr ,tree)
                                                    (,rec (cdr ,tree))
                                                    (if (functionp ,nill)
                                                        (funcall ,nill nil)
                                                        ,nill)))
                                      (list2
                                       (if sup-l (list `(,l-sym (car ,tree))))
                                       (if sup-r (list `(,r-sym (cdr ,tree))))))
                               g-form))
                            (t
                             (if (functionp ,leaf)
                                 (funcall ,leaf ,tree)
                                 ,leaf)))))
             (,rec ,tree))))))

;; (f-trec/macro ((fl fr) (1+ (max fl (1- fr)))) (0 -1))
;; is the function to calculate the max number of levels
;; e.g. '(1) has 1 and '((1 (2 (6)) 3) 4) has 4
;; Note that (1- fr) is to offset the counts on the right direction
;;
;; compare:
;;
;; (f-trec/macro ((fl fr) (1+ (max fl fr))) (-1 -1))
;; is the function to calculate the depth of tree, i.e. the max length from root
;; to leaves (the lowest cons being depth 0)
;; '(1) has 0 and '((1 (2 (6)) 3) 4) has 5
;; ( .-)-> (4 . nil)
;;  |
;; (1.-)-> ( .-)-> (3 . nil)
;;          |
;;         (2 .-)-> ( . nil)
;;                   |
;;                  (6 . nil)


(defun f-trec (g &optional (leaf-atom nil) (leaf-nil nil))
  (labels ((rec (tree g leaf-atom leaf-nil)
             (cond ((null tree) leaf-nil)
                   ((atom tree)
                    (if (functionp leaf-atom)
                        (funcall leaf-atom tree)
                        leaf-atom))
                   (t (funcall g
                               (rec (car tree) g leaf-atom leaf-nil)
                               (rec (cdr tree) g leaf-atom leaf-nil)
                               (car tree)
                               (cdr tree))))))
    #'(lambda (tree) (rec tree g leaf-atom leaf-nil))))




;; g(acc, car)
(defun f-foldl (g base)
  (labels ((rec (list g acc)
             (if (null list)
                 acc
                 (rec (cdr list) g (funcall g acc (car list))))))
    #'(lambda (list) (rec list g (if (functionp base)
                                     (funcall base)
                                     base)))))


;; FIXME: to declare ignore unused cons in let form
(defmacro f-foldl/macro (((acc car &optional (cons nil sup-cons)) g-form)
                         base-form)
  (with-gensyms (base list rec)
    `#'(lambda (,list)
         (let ((,base ,base-form))
           (labels ((,rec (,list ,acc) ;reuse the symbol ,list
                      (if (endp ,list)
                          ,acc
                          (,rec (cdr ,list)
                                (let ((,car (car ,list))
                                      ,@(if sup-cons
                                            (list `(,cons ,list))))
                                  ,g-form)))))
             (,rec ,list (if (functionp ,base)
                             (funcall ,base)
                             ,base)))))))




;;; the following requires anaphorics

;; it - car
;; rec - f(cdr)
;; where - cons
(defmacro f-lrec/m (g-form base-form &rest args)
  `(f-lrec/macro ((it rec where) ,g-form) ,base-form ,@args))

(defmacro on-cdrs (g-form base-form list &rest args)
  `(funcall (f-lrec/m ,g-form ,base-form ,@args) ,list))


;; this is reduce from right WITH initial value
;; Note that the recursion ends at (null list) at f-lrec/m
(defmacro f-foldr/m (&rest args)
  `(f-lrec/m ,@args))


(defmacro f-foldl/m (g-form base-form)
  `(f-foldl/macro ((acc it where) ,g-form) ,base-form))

(defmacro on-cars (g-form base-form list)
  `(funcall (f-foldl/m ,g-form ,base-form) ,list))


(defun unions (list &rest more-lists)
  (on-cars (union it acc) list more-lists))

(defun intersections (list &rest more-lists)
  (on-cars (intersect acc it) list more-lists))

(defun differences (list &rest more-lists)
  (on-cars (set-difference acc it) list more-lists))

(defun max-min (&rest lists)
  "Max of mins of lists."
  (on-cars (max acc (apply #'min it)) (apply #'min (car lists)) (cdr lists)))

(defun min-max (&rest lists)
  (on-cars (min acc (apply #'max it)) (apply #'max (car lists)) (cdr lists)))

