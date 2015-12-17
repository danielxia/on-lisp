;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; anaphoric macros
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defparameter *anaphoric-it* 'it)

(defun anaphoric-it ()
  (if (boundp *anaphoric-it*)
      *anaphoric-it*
      'it))

(defmacro with-anaphoric-it ((it) &body body)
  `(let ((,it (anaphoric-it)))
     ,@body))

;; we need to ensure that `test' is evaled without the newly binded it so it may
;; use `it' in the outer containing scope. See aif2 for similiar tricks.
(defmacro aif (test then &optional else)
  (once-only (test)
    (with-anaphoric-it (it)
      `(let ((,it ,test))
        (if ,it ,then ,else)))))

;; anaphoric variants
(defmacro awhen (test &body body)
  `(aif ,test
        (progn ,@body)))


(defmacro awhile (test &body body)
  (with-anaphoric-it (it)
    `(do ((,it ,test ,test))
         ((not ,it))
       ,@body)))

;; a chain of let-and-if
;; (aand (owner x) (address it) (town it))
;; this form retrieves the town of the owner when defined
(defmacro aand (&rest args)
  (cond ((null args) t)
        ((last? args) (car args))       ;avoid (and ... t)
        (t `(aif ,(car args) (aand ,@(cdr args))))))


(defmacro acond (&rest clauses)
  (if (null clauses)
      nil
      (let ((cl (car clauses)))
        (let-gensyms ((test-val (car cl)))
          `(if ,test-val
               ,(with-anaphoric-it (it)
                  `(let ((,it ,test-val))
                     ,@(cdr cl)))
               (acond ,@(cdr clauses)))))))

;; in the body of alambda, one can reference the lambda itself via self
(defmacro alambda (parms &body body)
  `(labels ((self ,parms ,@body))
     #'self))


;; test the second value and bind the first
;; convenient for gethash
;;
;; this version of aif2 does not cover test under the scope of `it' while the
;; textbook version does
;; So test can use `it' in the containing scope while `then' uses the newly
;; binded `it'; more natural chaining
(defmacro aif2 (test then &optional else)
  (with-gensyms (first second)
    (with-anaphoric-it (it)
      `(multiple-value-bind (,first ,second) ,test
         (if ,second
             
             (let ((,it ,first)) ,then)
             (let ((,it ,first)) ,else))))))

;; (or ,first ,second)
(defmacro aif-or (test then &optional else)
  (with-gensyms (first second)
    (with-anaphoric-it (it)
      `(multiple-value-bind (,first ,second) ,test
         (if (or ,first ,second)
             (let ((,it ,first)) ,then)
             (let ((,it ,first)) ,else))))))

(defmacro acond2 (&rest clauses)
  (cond ((null clauses) nil)
        ((in (caar clauses) 't 'otherwise) `(progn ,@(cdar clauses)))
        (t
         `(aif2 ,(caar clauses)
                (progn ,@(cdar clauses))
                (acond2 ,@(cdr clauses))))))

;; aif-or
(defmacro acond-or (&rest clauses)
  (cond ((null clauses) nil)
        ((in (caar clauses) 't 'otherwise) `(progn ,@(cdar clauses)))
        (t
         `(aif-or ,(caar clauses)
                (progn ,@(cdar clauses))
                (acond-or ,@(cdr clauses))))))
