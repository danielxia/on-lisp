;; macros
;; used by all modules


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; classic macros
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; (with-gensyms (var1 (var2 "gen1") (var3 :gen2)))
(defmacro with-gensyms (syms &body body)
  `(let ,(mapcar #'(lambda (sym)
                     (cond ((symbolp sym) `(,sym (gensym)))
                           ((consp sym)
                            (let ((name (if (symbolp (second sym))
                                            (symbol-name (second sym))
                                            (second sym))))
                              `(,(car sym) (gensym ,name))))
                           (t (error "Invalid with-gensyms args"))))
          syms)
     ,@body))


;; (let-gensyms ((sym init-form) sym2 ...) . `body)
;; don't forget the backquote around body
;; <=>
;; (with-gensyms (sym sym2...)
;;   `(let ((,sym ,init-form) ,sym2...) . body))
;;
;; let-gensyms wraps `body in a let form which binds ,sym to init-form
(defmacro let-gensyms (binds &body body)
  (let ((syms (remove-duplicates
               (mapcar #'(lambda (bind)
                           (cond ((symbolp bind) bind)
                                 ((and (consp bind) (symbolp (car bind)))
                                  (car bind))
                                 (t (error "Invalid binding syntax"))))
                       binds))))
    `(with-gensyms ,syms
       `(let (,,@(mapcar #'(lambda (bind)
                             (if (symbolp bind)
                                 ``,,bind
                                 (let ((sym (car bind))
                                       (init-form (cadr bind)))
                                   ``(,,sym ,,init-form))))
                         binds))
          ,,@body))))


;; (condlet (((= 1 2) (x (princ 'a)) (y (princ 'b)))
;;           ((= 1 1) (y (princ 'c)) (x (princ 'd)))
;;           (t
;;            (x (princ 'e)) (z (princ 'f))))
;;          (list x y z))
;;
;; The problem with this implementation is that it does not allow, e.g. y in
;; t branch to inherit value from lexically containing form
(defmacro condlet (clauses &body body)
  (labels (;; build a let binding form for one cond clause
           (condlet-binds (cl vars)
             (mapcar #'(lambda (bind)
                         (if (consp bind)
                             (cons (cdr (assoc (car bind) vars))
                                   (cdr bind))))
                     (cdr cl))))
    (let ((bodfn (gensym))
          (vars (mapcar #'(lambda (v) (cons v (gensym)))
                        (remove-duplicates
                         (mapcar #'car
                                 (mappend #'cdr clauses))))))
      `(labels ((,bodfn ,vars ,@body))
         (cond ,(mapcar #'(lambda (cl)
                            `(,(car cl)
                               (let ,(mapcar #'cdr vars)
                                 (let ,(condlet-binds cl vars)
                                   (,bodfn ,(mapcar #'cdr vars))))))
                        clauses))))))


;; (defmacro my-macro (arg1 arg2 &body body)
;;   (once-only (arg1)
;;      ...))
;; once-only will keep any ,arg1 in my-macro from evaling multiple times.
;; how it is done? study the following example. Note what ,start and ,end eval
;; to: the name of other temp vars that hold the form values
;;
;; gg holds the value of the form stored in arg1
;; g holds the symbol gg (so that ,g = 'gg)
;; we (let ((arg1 g)) ...) so now ,arg1 = 'gg
;; note in the expansion of do-primes, start and end are replaced by their 'gg
;;
;; (the once-only definition used in the example is from practical common lisp)
;;
;; [This is REPL]
;; * (defmacro do-primes ((var start end) &body body)
;;     (once-only (start end)
;;                `(do ((,var (next-prime ,start) (next-prime (1+ ,var))))
;;                     ((> ,var ,end))
;;                   ,@body)))
;; 
;; DO-PRIMES
;; * (macroexpand-1 '(once-only (start end) (body-form)))
;; 
;; (LET ((#:G587 (GENSYM)) (#:G588 (GENSYM)))
;;   `(LET (,`(,#:G587 ,START) ,`(,#:G588 ,END))
;;      ,(LET ((START #:G587) (END #:G588))
;;         (BODY-FORM))))
;; T
;; * (macroexpand-1 '(do-primes (x 100 900) (do-some-thing)))
;; 
;; (LET ((#:G589 100) (#:G590 900))
;;   (DO ((X (NEXT-PRIME #:G589) (NEXT-PRIME (1+ X))))
;;       ((> X #:G590))
;;     (DO-SOME-THING)))
;; T
;; 
(defmacro once-only (names &body body)
  (let ((gensyms (mapcar #'(lambda (_) (declare (ignore _)) (gensym))
                         names)))
    `(let ,(mapcar #'(lambda (g) `(,g (gensym)))
                   gensyms)
       `(let (,,@(mapcar #'(lambda (g name) ``(,,g ,,name))
                         gensyms
                         names))
          ,(let ,(mapcar #'(lambda (name g) `(,name ,g))
                         names
                         gensyms)
             ,@body)))))



(defmacro in (obj &rest choices)
  (with-gensyms (sym)
    `(let ((,sym ,obj))
       (or ,@(mapcar #'(lambda (ch) `(eql ,sym ,ch))
                     choices)))))

;; like setq, (inq (get obj) a b c d)
(defmacro inq (obj &rest args)
  `(in ,obj ,@(mapcar #'(lambda (arg) `(quote ,arg))
                      args)))

;; in-if
(defmacro in-if (fn &rest choices)
  (with-gensyms (sym)
    `(let ((,sym ,fn))
       (or ,@(mapcar #'(lambda (ch) `(funcall ,sym ,ch))
                     choices)))))

;; conditional key eval version of case
(defmacro >case (expr &rest clauses)
  (with-gensyms (v)
    `(let ((,v ,expr))
       (cond ,@(mapcar #'(lambda (cl) (>case/cl-build v cl))
                       clauses)))))

(defun >case/cl-build (v cl)
  (let ((key (car cl))
        (forms (cdr cl)))
    (cond ((consp key)
           `((in ,v ,@key) ,@forms))
          ((inq key t otherwise)
           `(t ,@forms))
          (t (error "bad >case clause")))))

;; iteration
(defmacro while (test &body body)
  `(block (loop (when (not ,test)
                  (return-from nil))
             ,@body)))

(defmacro till (test &body body)
  `(block (loop (when (,test)
                  (return-from nil))
             ,@body)))

(defmacro until (test &body body)
  `(block (loop ,@body
             (when (not ,test)
               (return-from nil)))))

(defmacro for ((var start stop) &body body)
  (with-gensyms (stop-sym)
    `(do ((,var ,start (1+ ,var))
          (,stop-sym ,stop))
         ((> ,var ,stop-sym))
       ,@body)))

