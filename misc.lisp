;;; ungrouped utilities

(defun memoize (fn &key (cache (make-hash-table :test #'equal)))
  "Memoize a function FN"
  #'(lambda (&rest args)
      (multiple-value-bind (val hit) (gethash args cache)
        (if hit
            val
            (setf (gethash args cache) (apply fn args))))))


(defun xor (a b)
  (and (or a b) (or (not a) (not b))))


(defun mkstr (&rest args)
  (with-output-to-string (s)
    (dolist (a args) (princ a s))))

(defun symb (&rest args)
  (values (intern (apply #'mkstr args))))


;; most-of: the majority of T or NIL
(defmacro most-of (&rest args)
  (let ((threshold (ash (length args) -1))
        (hits (gensym)))
    `(let ((,hits 0))
       (or ,@(mapcar #'(lambda (arg)
                         `(and ,arg (> (incf ,hits) ,threshold)))
                     args)))))

;; gensyms #:... are not interned by reader. Every time reader encounters
;; a #:... it creates a new symbol object.
(defun gensym? (s)
  (and (symbolp s)
       (not (symbol-package s))))
