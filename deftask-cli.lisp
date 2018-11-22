(defpackage #:deftask-cli
  (:use #:cl #:alexandria #:deftask-utils #:cffi #:deftask-sys)
  (:export #:main #:build-image))

(in-package #:deftask-cli)

(cl-interpol:enable-interpol-syntax)

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

;;; option parsing and args

(defun argv ()
  (opts:argv))

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defun command-to-opts-var-name (name)
    (etypecase name
      (keyword (case name
                 (:main '*main-opts*)
                 (t (intern (format nil "*COMMAND-~A-OPTS*" name) #.*package*))))
      (symbol name))))

(defmacro define-opts (name (&optional inherit) &body descriptions)
  (let ((name (command-to-opts-var-name name))
        (inherit (command-to-opts-var-name inherit)))
    `(defparameter ,name
       (let ((opts::*options* nil))
         (opts:define-opts
             ,@descriptions)
         ,(if inherit
              `(append opts::*options* ,inherit)
              'opts::*options*)))))

(defun command-opts (x)
  (typecase x
    (keyword (let ((name (command-to-opts-var-name x)))
               (when (and name (boundp name))
                 (symbol-value name))))
    (t x)))

(defun get-opts (opts &optional (argv (argv)))
  (let ((opts::*options* (command-opts opts)))
    (opts:get-opts argv)))

(defun describe-opts (opts &key prefix suffix usage-of args (stream *standard-output*))
  (let ((opts::*options* (command-opts opts)))
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

;;; conditions

(define-condition command-error (error)
  ((command :initarg :command :reader command-error-command)))

(define-condition unknown-option (command-error)
  ((underlying-error :initarg :underlying-error :reader underlying-error)))

(define-condition syntax-error (command-error)
  ((message :initarg :message :reader syntax-error-message)))

(defun syntax-error (command fmt &rest args)
  (error 'syntax-error
         :command command
         :message (apply #'format nil fmt args)))

(defmethod print-object ((x syntax-error) stream)
  (if (or *print-readably* *print-escape*)
      (call-next-method)
      (format stream "Syntax error: ~A" (syntax-error-message x))))

;;; help definitions

(defun program-name ()
  (first (last (cl-ppcre:split "/" (first (argv))))))

(defparameter *short-descriptions* nil)

(defmacro define-short-description (command short-description)
  `(progn
     (when (not (find ,command *short-descriptions* :key #'car))
       (setf *short-descriptions* (append *short-descriptions*
                                          (list (cons ,command ,short-description)))))
     ,command))

(defun short-description (command)
  (cdr (find command *short-descriptions* :key #'car)))

(defgeneric usage-of (command))

(defmethod usage-of (command)
  (format nil "~A ~A" (program-name) (string-downcase command)))

(defgeneric usage-args (command))

(defmethod usage-args (command)
  nil)

(defmacro define-usage-args (command args)
  `(defmethod usage-args ((x (eql ,command)))
     ,args))

(defgeneric print-help-prefix (command &optional stream))

(defmethod print-help-prefix (command &optional (stream *standard-output*))
  (write-string (short-description command) stream))

(defmethod print-help-prefix :after (command &optional (stream *standard-output*))
  (terpri stream))

(defmacro define-help-prefix (command description)
  `(defmethod print-help-prefix ((x (eql ,command)) &optional (stream *standard-output*))
     (write-string ,description stream)))

(defgeneric print-help-suffix (command &optional stream))

(defmethod print-help-suffix (command &optional (stream *standard-output*))
  (declare (ignore stream))
  nil)

(defmacro define-help-suffix (command description)
  `(defmethod print-help-suffix ((x (eql ,command)) &optional (stream *standard-output*))
     (write-string ,description stream)))

(defun print-help (command &key (stream *standard-output*) (prefix t) (suffix t))
  (when prefix
    (print-help-prefix command stream))
  (describe-opts command
                 :usage-of (usage-of command)
                 :args (usage-args command))
  (when suffix
    (print-help-suffix command stream)))

;;; main

(defmethod print-help-prefix ((command (eql :main)) &optional (stream *standard-output*))
  (write-string "The command line client for deftask.com." stream))

(defmethod usage-of ((command (eql :main)))
  (program-name))

(define-usage-args :main "<command> [<options>] [<args>]")

(defun token-generation-url (&optional (endpoint deftask:*endpoint*))
  (let ((endpoint-uri (quri:uri endpoint)))
    (quri:render-uri
     (quri:make-uri :host (cl-ppcre:regex-replace "^api[.]" (quri:uri-host endpoint-uri) "")
                    :path "/settings/tokens"
                    :defaults endpoint-uri))))

(defmethod print-help-suffix ((command (eql :main)) &optional (stream *standard-output*))
  (write-string "Available commands:" stream)
  (terpri stream)
  (dolist (pair *short-descriptions*)
    (format stream "  ~A~27T~A~%"
            (string-downcase (car pair))
            (cdr pair)))
  (terpri stream)
  (write-string #?"For help on any command, use `$((program-name)) <command> -h`"
                stream)
  (terpri stream)
  (terpri stream)
  (write-string #?"Before you use any command, you need the API access token:

1. sign up on deftask.com
2. create an access token by visiting $((token-generation-url))
3. set the access token: `$((program-name)) config token ACCESS_TOKEN`

You can also provide it via the command line option --token. This will override the default."
                stream)
  (terpri stream))

(define-opts :main ()
  (:name :help
         :description "show help text"
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
         :meta-var "PROJECT_ID"))

(defun parse-command (arg)
  (cond ((null arg) nil)
        ((zerop (length arg)) :invalid)
        ((char= (char arg 0) #\-) nil)
        ((cl-ppcre:scan "\\s" arg) nil)
        ((fboundp
          (find-symbol (format nil "COMMAND-~A" (string-upcase arg))
                       #.*package*))
         (intern (string-upcase arg) :keyword))
        (t :invalid)))

(defun show-help-p (argv)
  (find-if (lambda (arg)
             (or (string= arg "-h") (string= arg "--help")))
           argv))

(defun handle-command (command argv)
  (handler-case
      (let* ((fn (intern (format nil "COMMAND-~A" (string-upcase command))
                         #.*package*)))
        (funcall fn argv))
    (opts:unknown-option (e)
      (error 'unknown-option :command command :underlying-error e))))

(defvar *default-config-file* (merge-pathnames #p".deftaskrc" (home)))

(defun read-config ()
  (when (probe-file *default-config-file*)
    (safe-read-from-string (alexandria:read-file-into-string *default-config-file*))))

(defvar *config* nil)

(defun get-config-value (name)
  (getf *config* name))

(defun (setf get-config-value) (value name)
  (setf (getf *config* name) value))

(defun remove-config-value (name)
  (remf *config* name))

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

(defun endpoint ()
  (or (getenv "DEFTASK_ENDPOINT")
      (get-config-value :endpoint)
      deftask:*endpoint*))

(defun token ()
  (or (get-opt-value :token)
      (getenv "DEFTASK_TOKEN")
      (get-config-value :token)))

(defun project-id ()
  (when-let (str (or (get-opt-value :project)
                     (getenv "DEFTASK_PROJECT")
                     (get-config-value :project)))
    (parse-integer str)))

(defmacro with-token-and-project-id (&body body)
  `(let ((deftask:*token* (token))
         (deftask:*project-id* (project-id)))
     ,@body))

;;; config

(define-short-description :config "Get or set configuration")

(define-help-prefix :config "Get or set configuration for the deftask client")

(define-usage-args :config "[<name>] [<value>]")

(define-help-suffix :config #?"To see the current configuration, use `$((program-name)) config`.

To set a config value, use `$((program-name)) config <name> <value>`

To remove a config value, use `$((program-name)) config -r <name>`")

(define-opts :config ()
  (:name :remove
         :description "remove the given setting"
         :short #\r
         :long "remove"))

(defun command-config (argv)
  (with-options-and-free-args (:config argv)
    (let* ((name (second *free-args*))
           (value (third *free-args*)))
      (cond
        ((and (get-opt-value :remove) name)
         (remove-config-value (config-name-to-keyword name))
         (write-config))
        (name
         (let ((keyword (config-name-to-keyword name)))
           (setf (get-config-value keyword) value)
           (write-config)))
        (t (print-config))))))

(define-short-description :project-config "Get or set configuration for a project")

(define-usage-args :project-config "[name] [value]")

(define-opts :project-config (:main)
  (:name :remove
         :description "remove the given setting"
         :short #\r
         :long "remove"))

(defun command-project-config (argv)
  (with-options-and-free-args (:project-config argv)
    (let* ((name (second *free-args*))
           (value (third *free-args*))
           (project-id (project-id))
           (keyword (when name
                      (config-name-to-keyword (format nil "projects.~A.~A" project-id name)))))
      (unless project-id
        (error "No project-id given"))
      (cond
        ((and (get-opt-value :remove) name)
         (remove-config-value keyword)
         (write-config))
        (name
         (setf (get-config-value keyword) value)
         (write-config))
        (t (let ((config (loop
                            :with prefix := (format nil "PROJECTS.~A." project-id)
                            :for (key value) :on *config* :by #'cddr
                            :if (starts-with-subseq prefix (symbol-name key))
                            :append (list key value))))
             (print-config config)))))))

(defun project-key (keyword project-id)
  (let ((full-name (format nil "PROJECTS.~A.~A" project-id keyword)))
    (intern full-name :keyword)))

(defun get-project-config-value (keyword &optional (project-id (project-id)))
  (get-config-value (project-key keyword project-id)))

(defun (setf get-project-config-value) (value keyword &optional (project-id (project-id)))
  (setf (get-config-value (project-key keyword project-id)) value))

;;; projects

(define-short-description :projects "List projects")

(defun command-projects (argv)
  (declare (ignore argv))
  (let* ((deftask:*token* (token))
         (projects (deftask:get-projects)))
    (dolist (project projects)
      (termcolor:with-color (:style :bright)
        (format t "#~D" (assocrv :project-id project)))
      (format t " ~A~%" (assocrv :name project)))))

(define-short-description :new "Create a new task")

(define-usage-args :new "<title>")

(define-opts :new (:main)
  (:name :description
         :description "task description"
         :short #\d
         :long "description"
         :arg-parser #'identity
         :meta-var "DESCRIPTION")
  (:name :label
         :description "assign label to task (can be provided multiple times)"
         :short #\l
         :long "label"
         :arg-parser #'identity
         :meta-var "LABEL")
  (:name :assignee
         :description "name of assignee (can be provided multiple times)"
         :short #\a
         :long "assignee"
         :arg-parser #'identity
         :meta-var "ASSIGNEE"))

(defun command-new (argv)
  (with-options-and-free-args (:new argv)
    (with-token-and-project-id
      (unless (second *free-args*)
        (syntax-error :new "Missing <title>"))
      (let* ((title (second *free-args*))
             (label-names (get-opt-value :label t))
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
             (task (deftask:deftask title
                       :description (get-opt-value :description)
                       :label-ids label-ids
                       :assignee-ids assignee-ids)))
        (termcolor:with-color (:style :bright)
          (format t "#~D" (assocrv :task-id task)))
        (format t " ~A~%" (assocrv :title task))))))

(defun print-task (task &key highlight-unread times labels assignees description task-labels task-users)
  (termcolor:with-color (:style :bright)
    (format t "#~D" (assocrv :task-id task)))
  (format t "~:[ ✅~;~]~:[~; 🔵~] ~A~%"
          (string= (assocrv :state task) "open")
          (when highlight-unread (not (assocrv :read task)))
          (assocrv :title task))
  (when times
    (let* ((creator-id (assocrv :creator task))
           (creator (if task-users
                        (find creator-id task-users :key (assocrv-fn :user-id))
                        (deftask:get-user creator-id)))
           (created-at (local-time:parse-timestring (assocrv :created-at task)))
           (updated-at (local-time:parse-timestring (assocrv :updated-at task))))
      (termcolor:with-color (:style :dim)
        (format t "  Created by ~A ~A~%"
                (assocrv :name creator)
                (relative-time created-at)))
      (when (not (roughly-same-time-p created-at updated-at))
        (termcolor:with-color (:style :dim)
          (format t "  Last updated ~A~%" (relative-time updated-at))))))
  (when labels
    (when-let (label-ids (assocrv :label-ids task))
      (let* ((task-labels (mapcar (lambda (label-id)
                                    (find label-id task-labels :key (assocrv-fn :label-id)))
                                  label-ids))
             (task-label-names (mapcar (assocrv-fn :name) task-labels)))
        (termcolor:with-color (:style :dim)
          (format t "  Labels: ~{~A~^, ~}~%" task-label-names)))))
  (when assignees
    (when-let (assignee-ids (assocrv :assignee-ids task))
      (let* ((assignees (mapcar (lambda (assignee-id)
                                  (find assignee-id task-users :key (assocrv-fn :user-id)))
                                assignee-ids))
             (assignee-names (mapcar (assocrv-fn ':name) assignees)))
        (termcolor:with-color (:style :dim)
          (format t "  Assignees: ~{~A~^, ~}~%" assignee-names)))))
  (let ((task-description (assocrv :description task)))
    (when (and description (plusp (length task-description)))
      (terpri)
      (write-string task-description)
      (terpri))))

(define-short-description :ls "List tasks")

(define-help-prefix :ls "List tasks for a project

Filter and re-order tasks using -q and -o respectively.

✅ indicates a closed task, 🔵 an unread one.")

(define-opts :ls (:main)
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
  (:name :compact
         :description "show tasks in a compact style (oneline)"
         :long "compact")
  (:name :detailed
         :description "show tasks in a detailed style"
         :long "detailed"))

(defun command-ls (argv)
  (with-options-and-free-args (:ls argv)
    (with-token-and-project-id
      (let* ((order-by (or (get-opt-value :order-by)
                           (get-project-config-value :ls.order-by)
                           "recently-updated"))
             (page (or (get-opt-value :page) 1))
             (response (deftask:get-tasks :query (get-opt-value :query)
                                          :order-by order-by
                                          :page page
                                          :page-info t
                                          :resolve-labels t
                                          :resolve-users t))
             (tasks (assocrv :tasks response))
             (detail (let ((default-detail (get-project-config-value :ls.detail)))
                       (cond
                         ((get-opt-value :compact) :compact)
                         ((get-opt-value :detailed) :detailed)
                         ((equal default-detail "compact") :compact)
                         ((equal default-detail "detailed") :detailed)
                         (t :detailed))))
             (labels (assocrv :labels response))
             (members (assocrv :users response))
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
          (if (eq detail :detailed)
              (print-task task
                          :highlight-unread t
                          :times t
                          :labels t
                          :assignees t
                          :task-labels labels
                          :task-users members)
              (print-task task :highlight-unread t)))))))

(defun print-comment (comment &key task-users)
  (let* ((creator-id (assocrv :creator comment))
         (creator (find creator-id task-users :key (assocrv-fn :user-id))))
    (unless creator
      (error "Couldn't find member with id: ~A" creator-id))
    (termcolor:with-color (:style :dim)
      (format t "Comment #~A by ~A~%"
              (assocrv :comment-id comment)
              (assocrv :name creator)))
    (termcolor:with-color (:style :dim)
      (let ((created-at (local-time:parse-timestring (assocrv :created-at comment)))
            (updated-at (local-time:parse-timestring (assocrv :updated-at comment))))
        (format t "Created ~A" (relative-time created-at))
        (when (not (roughly-same-time-p created-at updated-at))
          (format t " (updated ~A)" (relative-time updated-at)))
        (terpri)))
    (terpri)
    (write-string (assocrv :body comment))
    (terpri)))

(define-short-description :show "View a task")

(define-usage-args :show "<task-id>")

(define-opts :show (:main))

(defun command-show (argv)
  (with-options-and-free-args (:main argv)
    (with-token-and-project-id
      (unless (second *free-args*)
        (syntax-error :show "Missing <task-id>"))
      (let* ((task-id (second *free-args*))
             (task (deftask:get-task task-id :resolve-labels t :resolve-users t :resolve-comments t))
             (comments (assocrv '(:rel :comments) task))
             (labels (assocrv '(:rel :labels) task))
             (users (assocrv '(:rel :users) task)))
        (print-task task
                    :times t
                    :labels t
                    :assignees t
                    :description t
                    :task-labels labels
                    :task-users users)
        (when comments
          (dolist (comment comments)
            (write-string "---")
            (terpri)
            (print-comment comment :task-users users)))
        (when (not (assocrv :read task))
          (deftask:read-task task-id))))))

;;; close a task

(define-short-description :close "Close a task")

(define-help-prefix :close "Close the given task")

(define-usage-args :close "<task-id>")

(define-opts :close (:main))

(defun command-close (argv)
  (with-options-and-free-args (:main argv)
    (with-token-and-project-id
      (unless (second *free-args*)
        (syntax-error :close "Missing <task-id>"))
      (deftask:close-task (second *free-args*)))))

;;; re-open a task

(define-short-description :open "Reopen a task")

(define-help-prefix :open "Reopen the given task")

(define-usage-args :open "<task-id>")

(define-opts :open (:main))

(defun command-open (argv)
  (with-options-and-free-args (:main argv)
    (with-token-and-project-id
      (unless (second *free-args*)
        (syntax-error :open "Missing <task-id>"))
      (deftask:open-task (second *free-args*)))))

;;; edit a task

(define-short-description :edit "Edit a task")

(define-usage-args :edit "<task-id>")

(define-opts :edit (:main)
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

(defun command-edit (argv)
  (with-options-and-free-args (:edit argv)
    (with-token-and-project-id
      (deftask:edit-task (second *free-args*)
                         :title (get-opt-value :title)
                         :description (get-opt-value :description)))))

;;; comment

(define-short-description :comment "Comment on a task")

(define-usage-args :comment "<task-id> <text>")

(define-opts :comment (:main))

(defun command-comment (argv)
  (unless (second argv)
    (syntax-error :comment "Missing <task-id> and <text>"))
  (unless (third argv)
    (syntax-error :comment "Missing <text>"))
  (with-token-and-project-id
    (let ((comment (deftask:comment (second argv) (third argv))))
      (format t "Created comment #~A~%" (assocrv :comment-id comment)))))

;;; edit comment

(define-short-description :edit-comment "Edit a comment")

(define-usage-args :edit-comment "<task-id> <comment-id> <text>")

(define-opts :edit-comment (:main))

(defun command-edit-comment (argv)
  (unless (second argv)
    (syntax-error :edit-comment "Missing <task-id>, <comment-id> and <text>"))
  (unless (third argv)
    (syntax-error :edit-comment "Missing <comment-id> and <text>"))
  (unless (fourth argv)
    (syntax-error :edit-comment "Missing <text>"))
  (with-token-and-project-id
    (deftask:edit-comment (second argv) (third argv) (fourth argv))
    (format t "Edited comment #~A~%" (third argv))))

;; pager

(defun launch-pager (name &rest args)
  (destructuring-bind (read-fd . write-fd)
      (pipe)
    (let ((pid (fork)))
      (if (plusp pid)
          ;; parent
          (progn
            (dup2 write-fd +stdout+)
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

(defvar *pager* "less -FRX")

(defmacro with-pager (name-and-args &body body)
  (with-gensyms (pager child-pid)
    `(let ((,pager ,name-and-args))
       (if (and ,pager (interactive-terminal-p))
           (let ((,child-pid (apply #'launch-pager ,pager)))
             (unwind-protect (progn ,@body)
               (force-output *standard-output*)
               (posix-close +stdout+)
               (waitpid ,child-pid (null-pointer) 0)))
           (progn ,@body)))))

(defun pager ()
  (cl-ppcre:split " "
                  (or (getenv "PAGER")
                      (get-config-value :pager)
                      *pager*)))

;;; main

(defun main ()
  (with-output-to-string (*error-output*)
    (cffi:load-foreign-library 'cl+ssl::libssl))
  (with-simple-restart (abort "Abort program")
    (let* ((*default-config-file* (merge-pathnames #p".deftaskrc" (home)))
           (*config* (read-config))
           (termcolor:*colorize* (interactive-terminal-p))
           (deftask:*endpoint* (endpoint))
           (exit-code 0))
      (with-pager (pager)
        (handler-case
            (let* ((argv (argv))
                   (command (parse-command (second argv))))
              (if (show-help-p argv)
                  (print-help (or command :main))
                  (case command
                    ((nil) (print-help :main))
                    (:invalid
                     (format t "Error: Invalid command ~S~%" (second argv))
                     (print-help :main :prefix nil))
                    (t (handle-command command (rest argv))))))
          (deftask:api-error (e)
            (princ e *error-output*)
            (terpri *error-output*)
            (setf exit-code 1))
          (unknown-option (e)
            (princ (underlying-error e) *error-output*)
            (terpri *error-output*)
            (print-help (command-error-command e) :stream *error-output* :prefix nil :suffix nil))
          (syntax-error (e)
            (princ e *error-output*)
            (terpri *error-output*)
            (print-help (command-error-command e) :stream *error-output* :prefix nil :suffix nil))))
      (force-output *error-output*)
      (exit exit-code))))

;;; image

(defun build-image (&optional (path "deftask"))
  (with-output-to-string (*error-output*)
    (cffi:close-foreign-library 'cl+ssl::libssl))
  (sb-ext:disable-debugger)
  (sb-ext:save-lisp-and-die path
                            :executable t
                            :save-runtime-options t
                            :toplevel #'deftask-cli:main))
