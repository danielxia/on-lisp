;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; destructuring
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;; here we first implement a destructuring bind on list

(defun build-dstruct-pairs/list (pattern var)
  "Build a list of bindings to be wrapped in let*. The generated bindings will
destructively modifify VAR except for its leaves."
  (declare (type symbol var))
  (labels ((valid-bind-symbol? (sym)
             (not (in sym '&rest '&body)))

           (is-rest? (sym)
             (in sym '&rest '&body))

           (rec (pattern var acc)
             (cond ((null pattern) acc)
                   ((atom pattern)
                    (if (valid-bind-symbol? pattern)
                        (nconc `((,pattern ,var)) acc)
                        (error "Invalid dstruct-bind syntax")))
                   ;; &rest
                   ((is-rest? (car pattern))
                    (let ((p (cadr pattern)))
                      (if p
                          (nconc `((,p ,var)) acc)
                          (error "Invalid dstruct-bind syntax after &rest"))))
                   ;; (consp pattern)
                   ((atom (car pattern))
                    (if (valid-bind-symbol? (car pattern))
                        (rec (cdr pattern)
                             var
                             (cons `(,(car pattern) (pop ,var))
                                   acc))
                        (error "Invalid dstruct-bind syntax")))
                   ;; (consp (car pattern))
                   (t
                    (if (valid-bind-symbol? (car pattern))
                        (rec (cdr pattern)
                             var
                             (with-gensyms (x)
                               (nconc (rec (car pattern)
                                           x
                                           ;; here we deliberately contains (car
                                           ;; pattern)
                                           `((,x (pop ,var) (quote ,(car pattern)))))
                                      acc)))
                        (error "Invalid dstruct-bind syntax"))))))
    (nreverse (rec pattern var nil))))

(defun build-dstruct-bindings/list (pattern var)
  (declare (type symbol var))
  (cons `(,var (,f-check ,var (quote ,pattern)))
        (mapcar #'(lambda (bind)
                    (if (gensym? (car bind))
                        `(,(car bind) (dstruct-check/list ,@(cdr bind)))
                        bind))
                (build-dstruct-pairs/list pattern var))))

(defun dstruct-shallow-match/list (x dlist)
  (labels ((is-rest? (sym)
             (in sym '&rest '&body))

           (rec (x dlist)
             (cond ((or (atom dlist)
                        (and (null dlist) (null x)))
                    t)
                   ((is-rest? (car dlist))
                    (and (symbolp (cadr dlist))
                         (null (cddr dlist))))
                   ;; here dlist must be consp
                   ((consp x)
                    ;; in cases like '(1 2 3) '(x (y . z) w), 2 and (y . z) is
                    ;; deferred to next level shallow match
                    (rec (cdr x) (cdr dlist)))
                   (t
                    nil))))
    (rec x dlist)))

(defun dstruct-check/list (x dlist)
  "Check whether X fits the dstruct-bind list DLIST in the first level. Return
a shallow copy of X if check passes; otherwise signal an error."
  (if (dstruct-shallow-match/list x dlist)
      (copy-list x)
      (prog1 (values)
          (error "Invalid dstruct-bind syntax"))))

;; pattern is a destructuring lambda list with only &rest or &body
;; if a binding var is _ in pattern, it is not bound

(defun build-dstruct-pairs (pattern var)
  "Build a list of bindings to wrapped in let*. The generated bindings does not
modifify VAR."
  (declare (type symbol var))
  (labels ((is-rest? (sym)
             (in sym '&rest '&body))

           (is-ignore? (sym)
             (in sym '_))

           (valid-bind-symbol? (sym)
             (and sym (not (is-rest? sym))))

           (rec (pattern var index acc)
             (declare (type symbol var))
             (macrolet ((with-bind-symbol ((sym val) &body body)
                          `(let ((,sym ,val))
                             (if (valid-bind-symbol? ,sym)
                                 (progn ,@body)
                                 (error "Invalid dstruct-bind syntax")))))
               (cond ((null pattern) acc)
                     ((atom pattern)
                      (with-bind-symbol (s pattern)
                        (if (is-ignore? s)
                            nil
                            `((,s (subseq ,var ,index))
                              ,@acc))))
                     ;; &rest
                     ((is-rest? (car pattern))
                      (with-bind-symbol (s (cadr pattern))
                        (if (is-ignore? s)
                            nil
                            `((,s (subseq ,var ,index))
                              ,@acc))))
                     ;; (consp pattern)
                     ((atom (car pattern))
                      (with-bind-symbol (s (car pattern))
                        (rec (cdr pattern)
                             var
                             (1+ index)
                             (if (is-ignore? s)
                                 acc
                                 (cons `(,s (elt ,var ,index))
                                       acc)))))
                     ;; (consp (car pattern))
                     (t
                      (with-bind-symbol (p (car pattern))
                        (rec (cdr pattern)
                             var
                             (1+ index)
                             (with-gensyms (x)
                               (nconc (rec p x 0 `((,x (elt ,var ,index)
                                                       (quote ,(car pattern)))))
                                      acc)))))))))
    (nreverse (rec pattern var 0 nil))))

(defun build-dstruct-bindings (pattern var)
  (cons `(,var (dstruct-check ,var (quote ,pattern)))
        (mapcar #'(lambda (bind)
                    (if (and (gensym? (car bind)) (cddr bind))
                        `(,(car bind) (dstruct-check ,@(cdr bind)))
                        bind))
                (build-dstruct-pairs pattern var))))

(defun dstruct-check (seq dlist)
  (if (dstruct-shallow-match seq dlist)
      seq
      (prog1 (values)
        (error "Invalid dstruct-bind syntax"))))

;; FIXME: does not deal with &rest
(defun dstruct-shallow-match (seq dlist)
  (and (typep seq 'sequence)
       (aif2 (list-length1 dlist)
             it
             (= (length seq) it))))

(defmacro dstruct-bind (pattern x &body body)
  (once-only (x)
    `(let* ,(build-dstruct-bindings pattern x)
       ,@body)))

(defmacro dstruct-bind/list (pattern x &body body)
  (once-only (x)
    `(let* ,(build-dstruct-bindings/list pattern x)
       ,@body)))


;; with-array
;; (with-matrix ((a b c)
;;               (d _ f)
;;               (g h _)) (make-array '(3 3))
;;   (list a b c d f g h))
(defmacro with-matrix (rows array &body body)
  (once-only (array)
    `(let (,@(mapcani #'(lambda (i row)
                          (mapcani #'(lambda (j x)
                                       (if (eq x '_)
                                           nil
                                           (list `(,x (aref ,array ,i ,j)))))
                                   row))
                      rows))
       ,@body)))

(defmacro with-array (pattern array &body body)
  (once-only (array)
    `(let ,(mapcar #'(lambda (pos)
                       `(,(car pos) (aref ,array ,@(cdr pos))))
            pattern)
       ,@body)))


(defmacro with-struct ((name . fields) struct &body body)
  (flet ((mkaccess (s f)
           (values (intern (format nil "~A-~A"
                                   (symbol-name s)
                                   (symbol-name f))))))
    (once-only (struct)
      `(let ,(mapcar #'(lambda (field)
                         `(,field (,(mkaccess name field) ,struct)))
              fields)
         ,@body))))

