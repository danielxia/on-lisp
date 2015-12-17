;;; reader macros

;; example of quote
;;
;; The result should be a CL object that corresponds to the SOURCE CODE
(set-macro-character #\'
  #'(lambda (stream char)
      (list 'quote (read stream t nil t))))


;; this #-dispatch reader macro recoginize #?obj as a lambda which always returns
;; the CL object obj
;; #?2 --> #'(lambda (&rest) 2)
;; #?(1 2 3) --> #'(lambda (&rest) (1 2 3))
(set-dispatch-macro-character #\# #\?
  #'(lambda (stream char1 char2)
      ;; note the backquote
      ;; reader macro is MACRO, and returns SOURCE CODE representation
      `#'(lambda (&rest _)
           ,(read stream t nil t))))

;; #[2 4] --> (2 3 4)
(set-macro-character #\]
  (get-macro-character #\)))

(set-dispatch-macro-character #\# #\[
  #'(lambda (stream char1 char2)
      (let* ((tuple (read-delimited-list #\] stream t))
             (step (or (third tuple) 1))
             (start (ceiling (first tuple)))
             (end (floor (second tuple))))
        (do* ((i start (+ step i))
              (acc nil))
             ((> i end) `,(nreverse acc))
          (push i acc)))))

;; a pattern
(let ((rdelim (get-macro-character #\)))) ;#\) is the default right parenthesis
                                        ; it could be configured
  (defun defdelim-fn (left right fn)
    (set-macro-character right rdelim)
    (set-dispatch-macro-character #\# left
      #'(lambda (stream char1 char2)
          (apply fn (read-delimited-list right stream t))))))

(defmacro defdelim (left right parms &body body)
  `(defdelim-fn ,left ,right #'(lambda ,parms ,@body)))


(defdelim #\[ #\] (start end &optional step)
  `,(mapa->b #'identity start end step))

;; #{f1 f2 f3} --> (fn (f. f1 f2 f3))
(defdelim #\{ #\} (&rest args)
  `(fn (f. ,@args)))

