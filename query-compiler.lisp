;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; a query compiler
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


(defun make-db (&optional (size 100))
  (make-hash-table :size size))

(defvar *default-db* (make-db))

(defun clear-db (&optional (db *default-db*))
  (clrhash db))

(defun db-query (key &optional (db *default-db*))
  (gethash key db))

(defun (setf db-query) (val key &optional (db *default-db*))
  (setf (gethash key db) val))

(defun db-push (key val &optional (db *default-db*))
  (push val (db-query key db)))

(defmacro fact (key &rest args)
  `(progn (db-push ',key ',args)
          ',args))


;; query lang:
;; query := (symbol arg*)
;;        | (not query)
;;        | (and query*)
;;        | (or query*)
;; arg := ?symbol
;;      | other atoms

;; types: and or not lisp t
(defun query-type (query)
  (car query))

(defmacro query-type-case (query &body cases)
  `(case (query-type ,query)
     ,@(mapcar #'(lambda (c)
                   `(,(car c) ,@(cdr c)))
        cases)))

(defun compile-query (query rest)
  (query-type-case query
    (and  (compile-and (cdr query) rest))
    (or   (compile-or (cdr query) rest))
    (not  (compile-not (cadr query) rest))
    (lisp `(if ,(cadr query) ,rest))
    (t    (compile-simple query rest))))

(defun compile-and (queries rest)
  (if (null queries)
      rest
      (compile-query (car queries)
                     (compile-and (cdr queries) rest))))

(defun compile-or (queries rest)
  (if (null queries)
      nil
      (with-gensyms (rest-f)
        (let ((vars (vars-in rest)))
          `(flet ((,rest-f ,vars ,rest))
             ,@(mapcar #'(lambda (q)
                           (compile-query q `(,rest-f ,@vars)))
                       queries))))))

(defun compile-not (query rest)
  (with-gensyms (tag)
    `(if (block ,tag
           ,(compile-query query `(return-from ,tag nil))
           t)
         ,rest)))

(defun compile-simple (query rest)
  (with-gensyms (record)
    `(dolist (,record (db-query (quote ,(car query))))
       (bind-match ,(cdr query) ,record ,rest nil))))

(defmacro with-answer (query &body body)
  `(with-gensyms ,(vars-in query)
     ,(compile-query query `(progn ,@body))))


(clear-db)
(fact painter hogarth william english)
(fact painter canale antonio venetian)
(fact painter reynolds joshua english)
(fact dates hogarth 1697 1772)
(fact dates canale 1697 1768)
(fact dates reynolds 1723 1792)
