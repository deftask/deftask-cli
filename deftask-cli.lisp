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
         :long "token"
         :arg-parser #'identity
         :meta-var "TOKEN")
  (:name :project
         :description "provide the project"
         :long "project"
         :arg-parser #'identity
         :meta-var "PROJECT"))

(defun parse-subcommand (arg)
  (cond ((null arg) nil)
        ((zerop (length arg)) :invalid)
        ((char= (char arg 0) #\-) nil)
        ((cl-ppcre:scan "\\s" arg) nil)
        ((fboundp
          (find-symbol (format nil "SUBCOMMAND-~A" (string-upcase arg))
                       #.*package*))
         (intern (string-upcase arg) :keyword))
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

(defvar *config* nil)

(defun get-config-value (name)
  (getf *config* name))

(defun (setf get-config-value) (value name)
  (setf (getf *config* name) value))

(defun write-config (&optional (config *config*))
  (with-open-file (out *default-config-file* :direction :output :if-exists :supersede)
    (with-standard-io-syntax
      (write config :stream out))))

(defun config-name-to-keyword (name)
  (intern (string-upcase name) :keyword))

(defun print-config (&optional (config *config*))
  (loop
     :for (key value) :on config :by #'cddr
     :do (format t "~&~A = ~A~%" (string-downcase key) value)))

(defun token ()
  (or (getf *options* :token)
      (getenv "DEFTASK_TOKEN")
      (get-config-value :token)))

(defun project-id ()
  (when-let (str (or (getf *options* :project)
                     (getenv "DEFTASK_PROJECT")
                     (get-config-value :project)))
    (parse-integer str)))

(defmacro with-token-and-project-id (&body body)
  `(let ((deftask:*token* (token))
         (deftask:*project-id* (project-id)))
     ,@body))

(defun subcommand-defaults (argv)
  (let* ((name (second argv))
         (value (third argv)))
    (if name
        (let ((keyword (config-name-to-keyword name)))
          (setf (get-config-value keyword) value)
          (write-config))
        (print-config))))

(defun subcommand-project-defaults (argv)
  (with-options-and-free-args (*main-opts* argv)
    (let* ((name (second *free-args* ))
           (value (third *free-args*))
           (project-id (project-id)))
      (unless project-id
        (error "No project-id given"))
      (if name
          (let ((keyword (config-name-to-keyword
                          (format nil "projects.~A.~A" project-id name))))
            (setf (get-config-value keyword) value)
            (write-config))
          (let ((config (loop
                           :with prefix := (format nil "PROJECTS.~A." project-id)
                           :for (key value) :on *config* :by #'cddr
                           :if (starts-with-subseq prefix (symbol-name key))
                           :append (list key value))))
            (print-config config))))))

(defun project-key (keyword project-id)
  (let ((full-name (format nil "PROJECTS.~A.~A" project-id keyword)))
    (intern full-name :keyword)))

(defun get-project-config-value (keyword &optional (project-id (project-id)))
  (get-config-value (project-key keyword project-id)))

(defun (setf get-project-config-value) (value keyword &optional (project-id (project-id)))
  (setf (get-config-value (project-key keyword project-id)) value))

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
  (:name :page
         :description "page to display"
         :short #\p
         :long "page"
         :arg-parser #'parse-integer
         :meta-var "PAGE")
  (:name :detail
         :description "task detail"
         :short #\d
         :long "detail"
         :arg-parser #'identity
         :meta-var "DETAIL"))

(defun subcommand-ls (argv)
  (with-options-and-free-args (*ls-opts* argv)
    (with-token-and-project-id
      (let* ((order-by (or (get-opt-value :order-by)
                           (get-project-config-value :order-by)
                           "updated-at-desc"))
             (page (or (get-opt-value :page) 1))
             (response (deftask:list-tasks :query (get-opt-value :query)
                                           :order-by order-by
                                           :page page))
             (tasks (assocrv :tasks response))
             (labels (when (some (assocrv-fn :label-ids) tasks)
                       (deftask:get-labels)))
             (members (when (some (assocrv-fn :assignee-ids) tasks)
                        (deftask:get-project-members deftask:*project-id*)))
             (detail (or (get-opt-value :detail)
                         (get-project-config-value :detail)))
             (page-info (assocrv :page-info response))
             (count (length tasks))
             (total-count (assocrv :count page-info))
             (current-page (assocrv :page page-info))
             (page-count (assocrv :page-count page-info)))
        (if (> page-count 1)
            (format t "Page ~A of ~A, showing ~A of ~A matching tasks~%"
                    current-page page-count
                    count total-count)
            (format t "~A tasks~%" count))
        (format t "Ordered by ~A~%" order-by)
        (format t "---~%")
        (dolist (task tasks)
          (termcolor:with-color (:style :bright)
            (format t "#~D" (assocrv :task-id task)))
          (format t "~:[ ✅~;~] ~A~%" (string= (assocrv :state task) "open") (assocrv :title task))
          (when (or (null detail) (string= detail "detailed"))
            (let* ((time-string (if (starts-with-subseq "updated-at" order-by)
                                    (assocrv :updated-at task)
                                    (assocrv :created-at task)))
                   (time (local-time:parse-timestring time-string)))
              (termcolor:with-color (:style :dim)
                (format t "  ~A: ~A~%"
                        (if (starts-with-subseq "updated-at" order-by) "Updated" "Created")
                        (relative-time time :sentencep nil))))
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

(defun subcommand-close (argv)
  (with-options-and-free-args (*main-opts* argv)
    (with-token-and-project-id
      (deftask:close-task (second *free-args*)))))

(defun subcommand-open (argv)
  (with-options-and-free-args (*main-opts* argv)
    (with-token-and-project-id
      (deftask:open-task (second *free-args*)))))

(defun subcommand-reopen (argv)
  (subcommand-open argv))

(define-opts *edit-task-opts* (*main-opts*)
  (:name :title
         :description "task title"
         :short #\t
         :long "title"
         :arg-parser #'identity
         :meta-var "TITLE")
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

(defun subcommand-edit (argv)
  (with-options-and-free-args (*edit-task-opts* argv)
    (with-token-and-project-id
      (deftask:edit-task (second *free-args*)
                         :title (get-opt-value :title)
                         :description (get-opt-value :description)))))

(defun subcommand-comment (argv)
  (with-token-and-project-id
    (let ((comment (deftask:comment (second argv) (third argv))))
      (format t "Created comment #~A~%" (assocrv :comment-id comment)))))

(defun subcommand-edit-comment (argv)
  (with-token-and-project-id
    (deftask:edit-comment (second argv) (third argv) (fourth argv))
    (format t "Edited comment #~A~%" (third argv))))

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
    (let ((*config* (read-config))
          (termcolor:*colorize* (interactive-terminal-p)))
      (with-pager ("less" (list "-FRX"))
        (let* ((argv (opts:argv))
               (subcommand (parse-subcommand (second argv))))
          (if (show-help-p argv)
              (show-help subcommand (rest argv))
              (case subcommand
                ((nil) (handle-no-subcommand (rest argv)))
                (:invalid (show-help subcommand (rest argv)))
                (t (handle-subcommand subcommand (rest argv))))))))))
