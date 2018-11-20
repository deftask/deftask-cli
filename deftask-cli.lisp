(defpackage #:deftask-cli
  (:use #:cl #:alexandria #:deftask-utils #:cffi #:deftask-sys)
  (:export #:main))

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

(defun print-help (command &optional (stream *standard-output*))
  (print-help-prefix command stream)
  (describe-opts command
                 :usage-of (usage-of command)
                 :args (usage-args command))
  (print-help-suffix command stream))

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
  (write-string #?"Before you use any command, you need to provide the access token:

1. sign up on deftask.com
2. create an access token by visiting $((token-generation-url))
3. set the access token: `$((program-name)) defaults token ACCESS_TOKEN`

You can also provide it via the command line option --token. This will override the default."
                stream)
  (terpri stream))

(define-opts :main ()
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
  (let* ((fn (intern (format nil "COMMAND-~A" (string-upcase command))
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

;;; defaults

(define-short-description :defaults "Get or set default settings")

(define-help-prefix :defaults "Get or set default settings for the deftask client")

(define-usage-args :defaults "<name> <value>")

(define-opts :defaults ()
  (:name :remove
         :description "remove the given setting"
         :short #\r
         :long "remove"))

(defun command-defaults (argv)
  (with-options-and-free-args (:defaults argv)
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

(define-short-description :project-defaults "Get or set default settings for a project")

(define-opts :project-defaults (:main)
  (:name :remove
         :description "remove the given setting"
         :short #\r
         :long "remove"))

(defun command-project-defaults (argv)
  (with-options-and-free-args (:project-defaults argv)
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

(defun print-task (task &key highlight-unread times labels assignees description project-labels project-members)
  (termcolor:with-color (:style :bright)
    (format t "#~D" (assocrv :task-id task)))
  (format t "~:[ ✅~;~]~:[~; ✉️ ~] ~A~%"
          (string= (assocrv :state task) "open")
          (when highlight-unread (not (assocrv :read task)))
          (assocrv :title task))
  (when times
    (let* ((creator-id (assocrv :creator task))
           (creator (if project-members
                        (assocrv :user (find creator-id project-members :key (assocrv-fn '(:user :user-id))))
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
      (setf project-labels (or project-labels (deftask:get-labels)))
      (let* ((task-labels (mapcar (lambda (label-id)
                                    (find label-id project-labels :key (assocrv-fn :label-id)))
                                  label-ids))
             (task-label-names (mapcar (assocrv-fn :name) task-labels)))
        (termcolor:with-color (:style :dim)
          (format t "  Labels: ~{~A~^, ~}~%" task-label-names)))))
  (when assignees
    (when-let (assignee-ids (assocrv :assignee-ids task))
      (setf project-members (or project-members
                                (deftask:get-project-members deftask:*project-id*)))
      (let* ((assignees (mapcar (lambda (assignee-id)
                                  (find assignee-id project-members :key (assocrv-fn '(:user :user-id))))
                                assignee-ids))
             (assignee-names (mapcar (assocrv-fn '(:user :name)) assignees)))
        (termcolor:with-color (:style :dim)
          (format t "  Assignees: ~{~A~^, ~}~%" assignee-names)))))
  (let ((task-description (assocrv :description task)))
    (when (and description (plusp (length task-description)))
      (terpri)
      (write-string task-description)
      (terpri))))

(define-short-description :ls "List tasks")

(define-help-prefix :ls "List tasks for a project

Filter and re-order tasks using -q|--query and -o|--order-by respectively.")

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
                           (get-project-config-value :order-by)
                           "recently-updated"))
             (page (or (get-opt-value :page) 1))
             (response (deftask:get-tasks :query (get-opt-value :query)
                                          :order-by order-by
                                          :page page
                                          :page-info t))
             (tasks (assocrv :tasks response))
             (detail (let ((default-detail (get-project-config-value :detail)))
                       (cond
                         ((get-opt-value :compact) :compact)
                         ((get-opt-value :detailed) :detailed)
                         ((equal default-detail "compact") :compact)
                         ((equal default-detail "detailed") :detailed)
                         (t :detailed))))
             (labels (when (and (eq detail :detailed)
                                (some (assocrv-fn :label-ids) tasks))
                       (deftask:get-labels)))
             (members (when (and (eq detail :detailed)
                                 (some (assocrv-fn :assignee-ids) tasks))
                        (deftask:get-project-members deftask:*project-id*)))
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
                          :project-labels labels
                          :project-members members)
              (print-task task :highlight-unread t)))))))

(defun print-comment (comment &key project-members)
  (let* ((creator-id (assocrv :creator comment))
         (creator (find creator-id project-members
                        :key (assocrv-fn '(:user :user-id)))))
    (unless creator
      (error "Couldn't find member with id: ~A" creator-id))
    (termcolor:with-color (:style :dim)
      (format t "Comment #~A by ~A~%"
              (assocrv :comment-id comment)
              (assocrv '(:user :name) creator)))
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
  (with-token-and-project-id
    (let* ((task-id (second argv))
           (task (deftask:get-task task-id))
           (comments (deftask:get-comments task-id))
           (project-members (deftask:get-project-members deftask:*project-id*)))
      (print-task task
                  :times t
                  :labels t
                  :assignees t
                  :description t
                  :project-members project-members)
      (when comments
        (dolist (comment comments)
          (termcolor:with-color (:style :dim)
            (write-string "---"))
          (terpri)
          (print-comment comment :project-members project-members)))
      (when (not (assocrv :read task))
        (deftask:read-task task-id)))))

;;; close a task

(define-short-description :close "Close a task")

(define-help-prefix :close "Close the given task")

(define-usage-args :close "<task-id>")

(define-opts :close (:main))

(defun command-close (argv)
  (with-options-and-free-args (:main argv)
    (with-token-and-project-id
      (deftask:close-task (second *free-args*)))))

;;; re-open a task

(define-short-description :open "Reopen a task")

(define-help-prefix :open "Reopen the given task")

(define-usage-args :open "<task-id>")

(define-opts :open (:main))

(defun command-open (argv)
  (with-options-and-free-args (:main argv)
    (with-token-and-project-id
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
  (with-token-and-project-id
    (let ((comment (deftask:comment (second argv) (third argv))))
      (format t "Created comment #~A~%" (assocrv :comment-id comment)))))

;;; edit comment

(define-short-description :edit-comment "Edit a comment")

(define-usage-args :edit-comment "<task-id> <comment-id> <text>")

(define-opts :edit-comment (:main))

(defun command-edit-comment (argv)
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

(defvar *pager* nil)
(defvar *default-pager* nil)

(defmacro with-pager (name-and-args &body body)
  (with-gensyms (pager child-pid)
    `(let ((,pager (or ,name-and-args *pager*)))
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
                      *default-pager*)))

;;; main

(defun main ()
  (with-simple-restart (abort "Abort program")
    (let ((*config* (read-config))
          (termcolor:*colorize* (interactive-terminal-p))
          (exit-code 0))
      (with-pager (pager)
        (handler-case
            (let* ((argv (argv))
                   (command (parse-command (second argv))))
              (if (show-help-p argv)
                  (print-help (or command :main))
                  (case command
                    ((nil) (print-help :main))
                    (:invalid (show-help command (rest argv)))
                    (t (handle-command command (rest argv))))))
          (deftask:api-error (e)
            (princ e *error-output*)
            (terpri *error-output*)
            (setf exit-code 1))))
      (force-output *error-output*)
      (exit exit-code))))
