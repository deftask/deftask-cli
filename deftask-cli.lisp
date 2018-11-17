(defpackage #:deftask-cli
  (:use #:cl #:alexandria #:deftask-utils #:cffi #:deftask-sys)
  (:export #:main))

(in-package #:deftask-cli)

;;; secure read
;;; https://letoverlambda.com/textmode.cl/guest/chap4.html#sec_6

(defvar safe-read-from-string-blacklist
  '(#\# #\|))

(let ((rt (copy-readtable nil)))
  (defun safe-reader-error (stream closech)
    (declare (ignore stream closech))
    (error "safe-read-from-string failure"))

  (dolist (c safe-read-from-string-blacklist)
    (set-macro-character
      c #'safe-reader-error nil rt))

  (defun safe-read-from-string (s &optional fail)
    (if (stringp s)
      (let ((*readtable* rt) *read-eval*)
        (handler-bind
          ((error (lambda (condition)
                    (declare (ignore condition))
                    (return-from
                      safe-read-from-string fail))))
          (read-from-string s)))
      fail)))

;;; utils

(defun getenv (name)
  #+sbcl (sb-posix:getenv name))

(defun home ()
  (user-homedir-pathname))

;;; option parsing

(defmacro define-opts (name (&optional inherit) &body descriptions)
  `(defparameter ,name
     (let ((opts::*options* nil))
       (opts:define-opts
         ,@descriptions)
       ,(if inherit
            `(append ,inherit opts::*options*)
            'opts::*options*))))

(defun get-opts (opts &optional (argv (opts:argv)))
  (let ((opts::*options* opts))
    (opts:get-opts argv)))

(defun describe-opts (opts &key prefix suffix usage-of args (stream *standard-output*))
  (let ((opts::*options* opts))
    (opts:describe :prefix prefix
                   :suffix suffix
                   :usage-of usage-of
                   :args args
                   :stream stream)))

(defvar *options* nil)
(defvar *free-args* nil)

(defmacro with-options-and-free-args ((opts argv) &body body)
  `(multiple-value-bind (*options* *free-args*)
       (get-opts ,opts ,argv)
     ,@body))

(defun getf-all (plist indicator)
  (loop
     :for head := (member indicator plist) :then (member indicator (cddr head))
     :while head
     :collect (second head)))

(defun get-opt-value (name &optional sequence)
  (if sequence
      (getf-all *options* name)
      (getf *options* name)))

;;; main

(define-opts *main-opts* ()
  (:name :help
         :description "print this help text"
         :short #\h
         :long "help")
  (:name :token
         :description "provide the access token"
         :short #\t
         :long "token"
         :arg-parser #'identity
         :meta-var "TOKEN")
  (:name :project
         :description "provide the project"
         :short #\p
         :long "project"
         :arg-parser #'identity
         :meta-var "PROJECT"))

(defvar *subcommands* (list :projects :project :new :ls :list :search :show :close :open :re-open :edit :comment :edit-comment :default))

(defun parse-subcommand (arg)
  (cond ((null arg) nil)
        ((zerop (length arg)) :invalid)
        ((char= (char arg 0) #\-) nil)
        ((cl-ppcre:scan "\\s" arg) nil)
        (#1=(find arg *subcommands* :test #'string-equal) #1#)
        (t :invalid)))

(defun show-help-p (argv)
  (find-if (lambda (arg)
             (or (string= arg "-h") (string= arg "--help")))
           argv))

(defun show-help (subcommand argv)
  (declare (ignore subcommand argv))
  (describe-opts *main-opts* :prefix "deftask-cli is THE program"))

(defun handle-no-subcommand (argv)
  argv)

(defun handle-subcommand (subcommand argv)
  (let* ((fn (intern (format nil "SUBCOMMAND-~A" (string-upcase subcommand))
                     #.*package*)))
    (funcall fn argv)))

(defvar *default-config-file* (merge-pathnames #p".deftaskrc" (home)))

(defun read-config ()
  (when (probe-file *default-config-file*)
    (safe-read-from-string (alexandria:read-file-into-string *default-config-file*))))

(defvar *config* (read-config))

(defun write-config (&optional (config *config*))
  (with-open-file (out *default-config-file* :direction :output :if-exists :supersede)
    (with-standard-io-syntax
      (write config :stream out))))

(defun subcommand-default (argv)
  (let* ((name (second argv))
         (value (third argv)))
    (check-type name string)
    (check-type value string)
    (let ((keyword (intern (string-upcase name) :keyword)))
      (setf (getf *config* keyword) value)
      (write-config))))

(defun token ()
  (or (getf *options* :token)
      (getenv "DEFTASK_TOKEN")
      (getf *config* :token)))

(defun project-id ()
  (when-let (str (or (getf *options* :project)
                     (getenv "DEFTASK_PROJECT")
                     (getf *config* :project)))
    (parse-integer str)))

(defmacro with-token-and-project-id (&body body)
  `(let ((deftask:*token* (token))
         (deftask:*project-id* (project-id)))
     ,@body))

(defun subcommand-projects (argv)
  (declare (ignore argv))
  (let* ((deftask:*token* (token))
         (projects (deftask:get-projects)))
    (dolist (project projects)
      (termcolor:with-color (:style :bright)
        (format t "#~D" (assocrv :project-id project)))
      (format t " ~A~%" (assocrv :name project)))))

(define-opts *new-task-opts* (*main-opts*)
  (:name :description
         :description "task description"
         :short #\d
         :long "description"
         :arg-parser #'identity
         :meta-var "DESCRIPTION")
  (:name :label
         :description "a label"
         :short #\l
         :long "label"
         :arg-parser #'identity
         :meta-var "LABEL")
  (:name :assignee
         :description "an assignee"
         :short #\a
         :long "assignee"
         :arg-parser #'identity
         :meta-var "ASSIGNEE"))

(defun subcommand-new (argv)
  (with-options-and-free-args (*new-task-opts* argv)
    (with-token-and-project-id
      (let* ((label-names (get-opt-value :label t))
             (label-ids (remove nil
                                (mapcar (lambda (name)
                                          (assocrv :label-id
                                                   (first (deftask:get-labels :name name))))
                                        label-names)))
             (assignee-names (get-opt-value :assignee t))
             (assignee-ids (remove nil
                                   (mapcar (lambda (name)
                                             (let ((members (deftask:get-project-members deftask:*project-id* name)))
                                               (assocrv '(:user :user-id) (first members))))
                                           assignee-names)))
             (task (deftask:deftask (second *free-args*)
                       :description (get-opt-value :description)
                       :label-ids label-ids
                       :assignee-ids assignee-ids)))
        (termcolor:with-color (:style :bright)
          (format t "#~D" (assocrv :task-id task)))
        (format t " ~A~%" (assocrv :title task))))))

(define-opts *ls-opts* (*main-opts*)
  (:name :query
         :description "search query"
         :short #\q
         :long "query"
         :arg-parser #'identity
         :meta-var "QUERY")
  (:name :order-by
         :description "field to order results by"
         :short #\o
         :long "order-by"
         :arg-parser #'identity
         :meta-var "ORDER_BY")
  (:name :detail
         :description "task detail"
         :short #\d
         :long "detail"
         :arg-parser (lambda (str)
                       (cond ((string= str "compact") :compact)
                             ((string= str "detailed") :detailed)
                             (t (error "DETAIL should be 'compact' or 'detailed'"))))
         :meta-var "DETAIL"))

(defun subcommand-ls (argv)
  (with-options-and-free-args (*ls-opts* argv)
    (with-token-and-project-id
      (let* ((tasks (deftask:list-tasks :query (get-opt-value :query)
                                        :order-by (get-opt-value :order-by)
                                        :page (get-opt-value :page)))
             (labels (when (some (assocrv-fn :label-ids) tasks)
                       (deftask:get-labels)))
             (members (when (some (assocrv-fn :assignee-ids) tasks)
                        (deftask:get-project-members deftask:*project-id*))))
        (dolist (task tasks)
          (termcolor:with-color (:style :bright)
            (format t "#~D" (assocrv :task-id task)))
          (format t " ~A~%" (assocrv :title task))
          (when (eq (get-opt-value :detail) :detailed)
           (when-let (label-ids (assocrv :label-ids task))
             (let* ((task-labels (mapcar (lambda (label-id)
                                           (find label-id labels :key (assocrv-fn :label-id)))
                                         label-ids))
                    (task-label-names (mapcar (assocrv-fn :name) task-labels)))
               (termcolor:with-color (:style :dim)
                 (format t "  Labels: ~{~A~^, ~}~%" task-label-names))))
           (when-let (assignee-ids (assocrv :assignee-ids task))
             (let* ((assignees (mapcar (lambda (assignee-id)
                                         (find assignee-id members :key (assocrv-fn '(:user :user-id))))
                                       assignee-ids))
                    (assignee-names (mapcar (assocrv-fn '(:user :name)) assignees)))
               (termcolor:with-color (:style :dim)
                 (format t "  Assignees: ~{~A~^, ~}~%" assignee-names))))))))))

;; less launcher

(defun launch-pager (name args)
  (destructuring-bind (read-fd . write-fd)
      (pipe)
    (let ((pid (fork)))
      (if (plusp pid)
          ;; parent
          (progn
            (dup2 write-fd +stdout+)
            (dup2 write-fd +stderr+)
            (posix-close read-fd)
            (posix-close write-fd)
            pid)
          ;; child
          (progn
            (dup2 read-fd +stdin+)
            (posix-close read-fd)
            (posix-close write-fd)
            (execvp name args))))))

(defun interactive-terminal-p ()
  (isatty +stdout+))

(defmacro with-pager ((name args) &body body)
  (let ((child-pid (gensym "CHILD-PID-")))
    `(if (interactive-terminal-p)
         (let ((,child-pid (launch-pager ,name ,args)))
           (unwind-protect (progn ,@body)
             (force-output *standard-output*)
             (force-output *error-output*)
             (posix-close +stdout+)
             (posix-close +stderr+)
             (waitpid ,child-pid (null-pointer) 0)))
         (progn ,@body))))

;;; main

(defun main ()
  (with-simple-restart (abort "Abort program")
    (let ((termcolor:*colorize* (interactive-terminal-p)))
      (with-pager ("less" (list "-FRX"))
        (let* ((argv (opts:argv))
               (subcommand (parse-subcommand (second argv))))
          (if (show-help-p argv)
              (show-help subcommand (rest argv))
              (case subcommand
                ((nil) (handle-no-subcommand (rest argv)))
                (:invalid (show-help subcommand (rest argv)))
                (t (handle-subcommand subcommand (rest argv))))))))))
