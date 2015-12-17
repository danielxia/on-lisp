;;; fn macro

;; (fn (. list 1+ truncate)) == (f. #'list #'1+ #'truncate)
;;
;; syntax of language
;; (fn lang)
;; lang = (. lang*) | (lang*) | symbol | (lambda ...)
(defmacro fn (expr)
  (with-gensyms (x)
    `(function (lambda (,x) ,(build-lang expr x)))))

(defun is-compose? (expr)
  (and (consp expr) (in (car expr) 'f. 'compose)))

;; literal means being valid operator, i.e. (lambda ...) or symbol
(defun is-literal? (expr)
  (or (symbolp expr)
      (and (consp expr) (eq 'lambda (car expr)))))

;; every built expression (i.e. the result of building lang) is a valid CL form
;; e.g. (and oddp plusp) --> (and (oddp x) (plusp x))
;;
;; call lang (op arg ...) --> ((build op) (build arg) ...)
;; compose lang (compose arg1 arg2 ...)
;;          --> ((build arg1) ((build arg2) ...))
;;          <=> (arg1 (arg2 ...))

;; (and oddp (f. 1+ 1+)) --> (and (oddp x) (1+ (1+ x)))
;;
;; (and (func1) (func2 farg1 farg2))
;; --> (and (func1 x) (func2 (farg1 (farg2 x))))
;; Note that in call lang, nested patterns should not be interpreted as
;; returning a function. (func1) -\-> (funcall (func1 x) x)
;; This is more natural
;;
;; (not (and oddp plusp)) --> (not (and (oddp x) (plusp x)))
;; (f. not (and oddp plusp)) --> (not ((lambda (x) (and (oddp x) (plusp x))) x))

;; it might seem identical to is-literal? but this function acts on built
;; expression
;; at current stage, only consider lambda and symbol as singly argumented
;; ideally, even though any compounded form which has single entry point
;; (e.g. (not (plusp x))) should be singly argumented
;;
;; Because this function does not correspond precisely to singly argumented
;; operators, we don't integrate into build-operator
(defun has-single-arg? (expr)
  (or (symbolp expr)
      (and (consp expr) (eq 'lambda (car expr)))))

;; build lang
;; the result is a singly argumented form, as indicated by the lambda wrapper in
;; (defmacro fn)
(defun build-lang (expr env)
  (cond ((is-literal? expr)
         `(,expr ,env))
        ((is-compose? expr)
         (build-compose (cdr expr) env))
        (t
         (build-call (car expr) (cdr expr) env))))

;; build lang as an operator
;; the diff is that literal expr will not be appended env
(defun build-operator (expr env)
  (cond ((is-literal? expr)
         expr)
        ((is-compose? expr)
         (build-compose (cdr expr) env))
        (t
         (build-call (car expr) (cdr expr) env))))

;; each fn is a sub-lang
(defun build-compose (fns env)
  (labels ((rec (fns)
             (if fns
                 (let ((form (build-operator (car fns) env)))
                   (if (has-single-arg? form)
                       `(,form ,(rec (cdr fns)))
                       ;; we need to wrap it in lambda because x can appear at
                       ;; multiple leaves in form
                       ;; since form is already argumented with name env
                       ;; we reuse that here in lambda
                       `((lambda (,env) ,form) ,(rec (cdr fns)))))
                 env)))
    (rec fns)))

;; If op is not an operator, then we need to funcall
(defun wrap-invoke (op args)
  (append (if (has-single-arg? op)
              (list op)
              `(funcall ,op))
          args))


(defun build-call (op fargs env)
  (let ((arg-forms
          (if (null fargs)
              (list env)
              (mapcar #'(lambda (farg)
                          (build-lang farg env))
                      fargs))))
    (if (is-literal? op)
        `(,op ,@arg-forms)
        (wrap-invoke (build-operator op env) arg-forms))))

;; the textbook version has a bug:
;; (fn (and (plusp) oddp))
;; --> (plusp) is not expanded to (plusp x) but left as it is

;; the textbook version has more lambda wrappings
