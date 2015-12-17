;;; bootstrap on-lisp utilities

(defparameter *on-lisp-base* '("core-macro"
                               "reader-macro"
                               "generalized-var"
                               "anaphorics"
                               "list"
                               "map"
                               "traverse"
                               "f-combine"
                               "fn"))

(defparameter *on-lisp-dir* "~/Dropbox/notes/programming/cl/on-lisp")

(defun on-lisp-load (part)
  (load (format nil "~A/~A.lisp" *on-lisp-dir* part)))

(dolist (f *on-lisp-base*)
  (on-lisp-load f))
