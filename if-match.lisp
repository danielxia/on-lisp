;; matching

;; match
;; (match '(?x y ?z) '(1 y ?w)) ==> '((?x . 1) (?z . ?w))
;; ? indicates a variable, othewise the symbol is a literal value
;; match returns the condition that two list are equal. The condition is stored
;; in BINDS since it is an elegant way to accumulate info.

;; (defun match (x y &optional binds)
;;   (acond-or
;;    ((or (eql x y) (eql x '_) (eql y '_))
;;     (values binds t))
;;    ;; above all consider equivalence (not only vars but also ordinary symbols)
;;    ((binding x binds) (match it y binds))
;;    ((binding y binds) (match x it binds))
;;    ;; vars
;;    ((varsym? x) (values (cons (cons x y) binds) t))
;;    ((varsym? y) (values (cons (cons y x) binds) t))
;;    ;; recurse
;;    ((and (consp x) (consp y)
;;          (match (car x) (car y) binds))
;;     (match (cdr x) (cdr y) it))
;;    (t
;;     (values nil nil))))

;; (defun binding (x binds)
;;   "Retrieve the root of all bindings of X in BINDS."
;;   (labels ((rec (x binds prev)
;;              (aif (assoc x binds)
;;                   (rec (cdr it) binds it)
;;                   (or prev nil))))
;;     (let ((b (rec x binds nil)))
;;       (values (cdr b) b))))

(defun varsym? (x)
  (and (symbolp x)
       (eq (char (symbol-name x) 0) #\?)))

(defun vars-in (expr &optional (atom? #'atom))
  (if (funcall atom? expr)
      (if (varsym? expr) (list expr))
      (union (vars-in (car expr) atom?)
             (vars-in (cdr expr) atom?))))


;; now we build a if-match that combines match and dstruct-bind
;; (if-match (?x b ?y) '(a b c)
;;           (list ?x ?y))
;;
;; (if-match1 (?x b ?y) '(a b c)
;;            (list x y))

(defun varsym->name (s)
  (intern (subseq (symbol-name s) 1)))

(defun build-if-match/list (pattern var then else
                            &optional (varsym-> #'varsym->name))
  `(let (,@(mapcar #'(lambda (v)
                       `(,(funcall varsym-> v) (quote ,(gensym))))
                   (vars-in pattern)))
     ,(build-bind-match/list pattern var then else varsym->)))

(defun build-bind-match/list (pattern var then else
                              &optional (varsym-> #'varsym->name))
  (let ((binds (build-dstruct-pairs/list pattern var)))
    (with-gensyms (else-f)
      `(flet ((unbound? (s)
                (and (symbolp s) (not (symbol-package s))))
              (,else-f () ,else))
         (if (dstruct-shallow-match/list ,var ',pattern)
             (let ((,var (copy-list ,var)))
               ,(reduce #'(lambda (bind acc)
                            (let ((s (car bind)))
                              (cond
                                ((eq s '_)
                                 `(progn ,(cadr bind) ,acc))
                                ((varsym? s)
                                 (let ((s (funcall varsym-> s))
                                       (elem (gensym)))
                                   `(let ((,elem ,(cadr bind)))
                                      (if (or (unbound? ,s)
                                              (equal ,s ,elem))
                                          (let ((,s ,elem))
                                            ,acc)
                                          (,else-f)))))
                                ((gensym? s)
                                 `(let ((,(car bind) ,(cadr bind)))
                                    (if (dstruct-shallow-match/list ,(car bind)
                                                                    ,(third bind))
                                        (let ((,(car bind) (copy-list ,(car bind)))))
                                        ,acc
                                        (,else-f))))
                                ;; literal match
                                (t
                                 `(if (equal (quote ,(car bind)) ,(cadr bind))
                                      ,acc
                                      (,else-f))))))
                        binds
                        :from-end t
                        :initial-value then))
             (,else-f))))))

(defun build-if-match (pattern var then else
                       &optional (varsym-> #'varsym->name))
  (let ((binds (build-dstruct-pairs pattern var)))
    (with-gensyms (else-f)
      `(let (,@(mapcar #'(lambda (v)
                           `(,(funcall varsym-> v) (quote ,(gensym))))
                       (vars-in pattern)))
         (flet ((unbound? (s)
                  (and (symbolp s) (not (symbol-package s))))
                (,else-f () ,else))
           ,(reduce #'(lambda (bind acc)
                        (let ((s (car bind)))
                          (cond
                            ((eq s '_) acc)
                            ((varsym? s)
                             (let ((s (funcall varsym-> s))
                                   (elem (gensym)))
                               `(let ((,elem ,(cadr bind)))
                                  (if (or (unbound? ,s)
                                          (equal ,s ,elem))
                                      (let ((,s ,elem))
                                        ,acc)
                                      (,else-f)))))
                            ((gensym? s)
                             `(let ((,(car bind) ,(cadr bind)))
                                (if (dstruct-shallow-match ,(car bind)
                                                           ,(third bind))
                                    ,acc
                                    (,else-f))))
                            ;; literal match
                            (t
                             `(if (equal ,(car bind) ,(cadr bind))
                                  ,acc
                                  (,else-f))))))
                    binds
                    :from-end t
                    :initial-value then))))))


(defmacro if-match1 (pattern x then &optional else)
  (once-only (x)
    (build-if-match pattern x then else)))

(defmacro if-match (pattern x then &optional else)
  (once-only (x)
    (build-if-match/list pattern x then else #'identity)))

;; all the vars in pattern are not bounded to gensyms
(defmacro bind-match (pattern x then &optional else)
  (once-only (x)
    (build-bind-match/list pattern x then else #'identity)))
