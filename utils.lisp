(defpackage #:deftask-utils
  (:use #:cl)
  (:export #:assocr #:assocrv #:assocrv-fn
           #:relative-time))

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

;;; time

(defvar *locale* "en")
(defvar *default-locale* "en")

(defun find-locale (name)
  (or (ignore-errors (cl-l10n:locale (substitute #\_ #\- name)))
      (cl-l10n:locale *default-locale*)))

(defun relative-time (t1 &key (t2 (local-time:now)) (locale *locale*) (sentencep t))
  (let ((diff (local-time:timestamp-difference t2 t1)))
    (cond ((minusp diff) ; in the future
           (if sentencep "just now" "now"))
          ((< diff 120)  ; < 2 mins
           (if sentencep "just now" "now"))
          ((< diff local-time:+seconds-per-hour+) ; < 1 hour
           (format nil "~Am~:[~; ago~]" (floor (/ diff local-time:+seconds-per-minute+)) sentencep))
          ((< diff local-time:+seconds-per-day+) ; 1 day
           (format nil "~Ah~:[~; ago~]" (floor (/ diff local-time:+seconds-per-hour+)) sentencep))
          ((or (< diff (* 180 local-time:+seconds-per-day+))
               (= (local-time:timestamp-year t1) (local-time:timestamp-year t2)))
           (cl-l10n:with-locale (find-locale locale)
             ;; FIXME: pattern should be locale specific
             (format nil "~:[~;on ~]~A" sentencep (cl-l10n:format-date nil t1 :pattern "d MMM"))))
          (t
           (cl-l10n:with-locale (find-locale locale)
             ;; FIXME: pattern should be locale specific
             (format nil "~:[~;in ~]~A" sentencep (cl-l10n:format-date nil t1 :pattern "MMM yyyy")))))))
