;;; lazy evaluation

;; delay force
(defconstant +unforced+ (gensym))

(defstruct delay forced closure)

(defmacro delay (expr)
  (let-gensyms ((delayed (make-delay :forced unforced)))
    `(setf (delay-closure ,delayed)
           #'(lambda ()
               (setf (delay-forced ,delayed) ,expr)))
    ,delayed))

(declaim (inline forcedp))
(defun forcedp (x)
  (and (delay-p x)                      ;non-delay object is forced because it
                                        ;is evaled immediately
       (eq (delay-forced x) +unforced+)))

(defun force (x)
  (if (delay-p x)
      (if (forcedp x)
          (delay-forced x)
          (funcall (delay-closure x)))
      x))


