(defpackage #:deftask-utils
  (:use #:cl)
  (:export #:assocr #:assocrv #:assocrv-fn))

(in-package #:deftask-utils)

;;; alist utils

(defun assocr (path alist &key (key #'identity) (test #'eql))
  (labels ((execute (path alist result)
             (cond
               ((null path)
                result)
               ((atom path)
                (assoc path alist :key key :test test))
               (t
                (let ((next (assoc (first path) alist :key key :test test)))
                  (execute (cdr path) (cdr next) next))))))
    (execute path alist nil)))

(defun assocrv (path alist &key (key #'identity) (test #'eql))
  (cdr (assocr path alist :key key :test test)))

(defun assocrv-fn (path &key (key #'identity) (test #'eql))
  (lambda (alist)
    (assocrv path alist :key key :test test)))


