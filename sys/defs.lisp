(cl:in-package #:deftask-sys)

;; string handling

(defun lisp-strings-to-foreign (strings pointer &key null-terminated-p)
  (let ((length (length strings)))
    (dotimes (i length pointer)
      (setf (mem-aref pointer :string i) (elt strings i)))
    (when null-terminated-p
      (setf (mem-aref pointer :string length) (null-pointer)))))

(defmacro with-lisp-strings-to-foreign ((pointer strings &key null-terminated-p) &body body)
  (let ((strings-var (gensym "STRINGS-")))
    `(let ((,strings-var ,strings))
       (with-foreign-object (,pointer :string (1+ (length ,strings-var)))
         (lisp-strings-to-foreign ,strings-var ,pointer :null-terminated-p ,null-terminated-p)
         ,@body))))

;; error handling

(defcvar "errno" :int)

(defcfun "strerror" :string
  (errnum :int))

(define-condition sys-error (error)
  ((number :initarg :number :reader sys-error-number)
   (foreign-name :initarg :foreign-name)))

(defun sys-error-message (error)
  (strerror (slot-value error 'number)))

(defun sys-error-name (error)
  (foreign-enum-keyword 'error-number (sys-error-number error)))

(defmethod print-object ((x sys-error) stream)
  (with-slots (number foreign-name)
      x
    (flet ((print-info ()
             (format stream "~A(~A): ~A" foreign-name number (sys-error-message x))))
      (if *print-escape*
          (print-unreadable-object (x stream :type t)
            (print-info))
          (print-info)))))

(defun sys-error-p (value type)
  (ecase type
    (:int (= value -1))
    (:string (null value))))

(defmacro with-error-signalling ((foreign-name &optional (type :int)) &body body)
  (let ((retval (gensym "RETVAL-")))
    `(let ((,retval (progn ,@body)))
       (when (sys-error-p ,retval ,type)
         (error 'sys-error :number *errno* :foreign-name ,foreign-name))
       ,retval)))

(export '(sys-error sys-error-name sys-error-number sys-error-message))

;; helper macro

(defmacro defsysfun (name-and-options return-type &body arguments)
  (setf name-and-options (if (atom name-and-options)
                             (list name-and-options)
                             name-and-options))
  (let* ((keyword-position (or (position-if #'keywordp name-and-options)
                               (length name-and-options)))
         (name (subseq name-and-options 0 keyword-position))
         (options (subseq name-and-options keyword-position))
         (signal-errors (getf options :signal-errors)))
    (remf options :signal-errors)
    (multiple-value-bind (lisp-name foreign-name spec)
        (cffi::parse-name-and-options (append name options))
      (let ((underlying-name (intern (concatenate 'string "%" (string lisp-name))
                                     #.*package*)))
        `(progn
           (defcfun (,underlying-name ,foreign-name ,@spec) ,return-type
             ,@arguments)
           (defun ,lisp-name ,(mapcar #'first arguments)
             (flet ((run ()
                      (apply (function ,underlying-name) (list ,@(mapcar #'first arguments)))))
               ,(if signal-errors
                    `(with-error-signalling (,foreign-name ,(if (eql signal-errors t) return-type signal-errors))
                       (run))
                    `(run))))
           (export ',lisp-name))))))

;;; definitions

(export '(pid termios oflag mode))

(defconstant +stdin+ 0)
(defconstant +stdout+ 1)
(defconstant +stderr+ 2)

(export '(+stdin+ +stdout+ +stderr+))

#+sbcl
(defun fork ()
  (sb-posix:fork))
(export 'fork)

(defsysfun "getpid" pid)

(defsysfun ("getpgid" :signal-errors :int) pid
  (pid pid))

(defsysfun ("setpgid" :signal-errors t) :int
  (pid pid)
  (pgid pid))

(defsysfun ("waitpid" :signal-errors :int) pid
  (pid pid)
  (stat-loc :pointer)
  (options :int))

#+sbcl
(progn
  (defun wifexited (status)
    (sb-posix:wifexited status))
  (defun wifsignaled (status)
    (sb-posix:wifsignaled status))
  (defun wifstopped (status)
    (sb-posix:wifstopped status))
  (defun wexitstatus (status)
    (sb-posix:wexitstatus status))
  (defun wtermsig (status)
    (sb-posix:wtermsig status))
  (defun wstopsig (status)
    (sb-posix:wstopsig status)))

(export '(wifexited wifsignaled wifstopped wexitstatus wtermsig wstopsig))

(defsysfun ("tcgetattr" :signal-errors t) :int
  (filedes :int)
  (termios (:pointer (:struct termios))))

(defsysfun ("tcsetattr" :signal-errors t) :int
  (filedes :int)
  (actions :int)
  (termios (:pointer (:struct termios))))

(export '+tcsadrain+)

(defsysfun ("tcgetpgrp" :signal-errors :int) pid
  (filedes :int))

(defsysfun ("tcsetpgrp" :signal-errors t) :int
  (filedes :int)
  (pgid pid))

(defsysfun "isatty" :boolean
  (filedes :int))

(defcfun (%execvp "execvp") :int
  (file :string)
  (argv (:pointer :string)))

(defun execvp (file args)
  (with-error-signalling ("execvp")
    (with-lisp-strings-to-foreign (argv (cons file args) :null-terminated-p t)
      (%execvp file argv))))
(export 'execvp)

(defsysfun "dup2" :int
  (filedes :int)
  (filedes2 :int))

(defun posix-open (path flags &optional (mode nil modep))
  (with-error-signalling ("open")
    (if modep
        (foreign-funcall "open" :string path :int flags mode mode :int)
        (foreign-funcall "open" :string path :int flags :int))))
(export 'posix-open)

(defsysfun (posix-close "close" :signal-errors t) :int
  (filedes :int))

(defun oflag (keyword)
  (foreign-enum-value 'oflag keyword))

(defun mode (keyword)
  (foreign-enum-value 'mode keyword))

(defcfun (%signal "signal") :pointer
  (sig :int)
  (func :pointer))

(defun sig (keyword)
  (foreign-enum-value 'sig keyword))

(defun posix-signal (name func)
  (%signal (sig name)
           (case func
             ((:default :dfl) (make-pointer +sig-dfl+))
             ((:ignore :ign) (make-pointer +sig-ign+))
             (t func))))
(export 'posix-signal)

(defcfun (%kill "kill") :int
  (pid pid)
  (sig :int))

(defun kill (pid name)
  (let ((number (sig name)))
    (with-error-signalling ("kill")
      (%kill pid number))))
(export 'kill)

(defcfun (%pipe "pipe") :int
  (filedes (:pointer :int)))

(defun pipe ()
  (with-foreign-object (filedes :int 2)
    (with-error-signalling ("pipe")
      (%pipe filedes))
    (cons (mem-aref filedes :int 0)
          (mem-aref filedes :int 1))))
(export 'pipe)

;; file system and dir

(defsysfun ("getwd" :signal-errors t) :string
  (buf :string))

(defsysfun ("chdir" :signal-errors t) :int
  (path :string))
