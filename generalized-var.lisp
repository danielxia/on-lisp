;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; generalized variables
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; define-modify-macro
;; (define-modify-macro macro-name (args-besides-g-var) func)
;; the g-var will have as new value (funcall func args-besides-g-var)

;; (togglef x)
;; <=>
;; x will have new value (not x)
(define-modify-macro togglef2 () not)

(define-modify-macro concf (tail) nconc)

(define-modify-macro cdrf () cdr)

(defmacro allf (value &rest places)
  (once-only (value)
    `(setf ,@(mapcan #'(lambda (pl) (list pl value))
                     places))))

(defmacro nilf (&rest places)
  `(allf nil ,@places))

(defmacro tf (&rest places)
  `(allf t ,@places))

(defmacro togglef (&rest places)
  `(progn ,@(mapcar #'(lambda (pl) `(togglef2 ,pl))
                    places)))

;; get-setf-expansion
;;
;; > (get-setf-expansion ’(aref a (incf i)))
;; (#:G4 #:G5)
;; (A (INCF I))
;; (#:G6)
;; (SYSTEM:SET-AREF #:G6 #:G4 #:G5)
;; (AREF #:G4 #:G5)
;;
;; incf:
;; (let* ((#:g4 a)
;;        (#:g5 (incf i))
;;        (#:g6 (1+ (aref #:g4 #:g5))))
;;   (system:set-aref #:g6 #:g4 #:g5))
;;
;; Any compound form is a valid place, since any compound form whose operator f
;; has no setf expander are expanded into a call to (setf f)


(defmacro bind-setf-expansion (place (dummies val-forms newval setter getter)
                               &body body)
  `(multiple-value-bind (,dummies ,val-forms ,newval ,setter ,getter)
       (get-setf-expansion ,place)
     `(let ,(mapcar #'list ,dummies ,val-forms)
        ,,@body)))


(defmacro _f (op place &rest args)
  (bind-setf-expansion place (dummies val-forms newval setter getter)
    `(let ((,(car newval) (,op ,getter ,@args)))
       ,setter)))


