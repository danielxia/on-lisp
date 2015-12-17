;; traversal

;; dolist with index
(defmacro dolist2 ((var index lst &optional res) &body body)
  `(do ((,var ,lst (cdr ,var))
        (,index 0 (1+ ,index)))
       ((null ,var)
        ,res)
     ,@body))

;; (do-tuples ((a b c) lst 2) . body)
(defmacro do-tuples ((parms source &optional (step 1)) &body body)
  (once-only (source)
    `(prog1 nil
       (list-map #'(lambda (_ ,@parms)
                     (declare (ignore _))
                     ,@body)
                 (list ,@(map0->n #'(lambda (n)
                                      `(nthcdr ,n ,source))
                                  (1- (length parms))))
                 ,step nil t))))


(defmacro do-tuples/c ((parms source) &body body)
  (let ((first (car parms)))
    (once-only (source)
      `(prog1 nil
         (do ((,first ,source (cdr ,first))
              ,@(mapcar #'(lambda (p)
                            `(,p (cdrw ,source ,source)
                                 (cdrw ,p ,source)))
                        (cdr parms)))
             ((null ,first))
           (let ,(mapcar #'(lambda (p) `(,p (car ,p))) parms)
             ,@body))))))


;; mvdo
;; multiple value assignment in do
;; (mvdo* ((x 1 (1+ x))
;;         ((y z) (values 0 0) (values z x)))
;;     ((> x 5) (list x y z))
;;   (princ (list x y z)))

;; (mvlet* ((x 1)
;;          ((y z) (values 2 3)))
;;   (list x y z))

;; each bind clause is allowed to have more than two elements. they are just
;; ignored. that is, (mvlet* ((x 1 ...)) . body) <=> (mvlet* ((x 1)) . body)
;; this FEATURE makes it capable of handling bindings in do forms without
;; modification
(defmacro mvlet* (binds &body body)
  (labels ((bind-gen (bind body)
             (cond ((null bind) (error "A binding must not be nil"))
                   ((consp bind)
                    (if (consp (car bind))
                        `(multiple-value-bind ,(car bind) ,(cadr bind)
                           ,@body)
                        `(let ((,(car bind) ,(cadr bind)))
                           ,@body)))
                   (t
                    (error "Invalid binding syntax for mvlet*"))))
           (rec (binds body)
             (let ((bind (car binds))
                   (rest (cdr binds)))
               (if (atom rest)
                   (bind-gen bind body)
                   (bind-gen bind (list (rec rest body)))))))
    (if (null binds)
        `(let () ,@body)
        (rec binds body))))


(defun mvdo-step-gen (parm-cls)
  (cond ((null parm-cls) nil)
        ((< (length (car parm-cls)) 3)
         (mvdo-step-gen (cdr parm-cls)))
        (t
         (cons (list (if (consp (caar parm-cls))
                         'multiple-value-setq
                         'setf)
                     (caar parm-cls)
                     (third (car parm-cls)))
               (mvdo-step-gen (cdr parm-cls))))))


(defmacro mvdo* (parm-cls (test-cl &rest result-forms) &body body)
  (with-gensyms (label)
    `(mvlet* ,parm-cls
       (block nil
         (tagbody
            ,label
            (when ,test-cl (return (progn ,@result-forms)))
            ,@body
            ,@(mvdo-step-gen parm-cls)
            (go ,label))))))
