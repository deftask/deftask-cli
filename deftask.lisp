(defpackage #:deftask
  (:use #:cl #:deftask-utils)
  (:export #:*endpoint* #:*token* #:*version*
           #:current-user #:get-user
           #:get-org #:get-org-members #:get-orgs
           #:*project-id* #:get-project #:get-project-members #:get-projects
           #:deftask #:get-tasks #:get-task #:close-task #:open-task #:edit-task
           #:get-comments #:comment #:edit-comment
           #:get-labels
           #:api-error))

(in-package #:deftask)

(cl-interpol:enable-interpol-syntax)

;;; sdk

(defvar *endpoint* "http://api.deftask-local.com:7680")

(defvar *token* nil)

(defvar *version* "0.1")

(define-condition api-error (error)
  ((status-code :initarg :status-code :reader api-error-status-code)
   (reason :initarg :reason :reader api-error-reason)
   (method :initarg :method :reader api-error-method)
   (path :initarg :path :reader api-error-path)))

(defmethod print-object ((x api-error) stream)
  (if (or *print-readably* *print-escape*)
      (call-next-method)
      (format stream "API error: HTTP status ~A ~S returned for ~A ~A"
              (api-error-status-code x)
              (api-error-reason x)
              (api-error-method x)
              (api-error-path x))))

(defun api-request (method path &optional parameters)
  (setf parameters (remove nil parameters :key #'cdr))
  (let ((url (quri:render-uri
              (quri:merge-uris (quri:make-uri :query `(("v" . ,*version*))
                                              :defaults (quri:uri path))
                               (quri:uri *endpoint*))))
        (drakma:*text-content-types* (cons '("application" . "json") drakma:*text-content-types*))
        (content (when (member method '(:post :patch))
                   (quri:url-encode-params parameters))))
    (multiple-value-bind (body status-code headers response-uri stream closedp reason)
        (drakma:http-request url
                             :method method
                             :basic-authorization (list "bearer" *token*)
                             :parameters (when (eql method :get) parameters)
                             :content content)
      (declare (ignore response-uri stream closedp))
      (if (and (>= status-code 200) (< status-code 300))
          (let ((content-type (cdr (assoc :content-type headers))))
            (when (alexandria:starts-with-subseq "application/json" content-type)
              (json:decode-json-from-source body)))
          (error 'api-error
                 :status-code status-code
                 :reason reason
                 :method method
                 :path path)))))

(defun current-user ()
  (api-request :get "/user"))

(defun get-user (user-id)
  (api-request :get #?"/users/$(user-id)"))

(defun get-org (org-id)
  (api-request :get #?"/orgs/$(org-id)"))

(defun get-org-members (org-id &optional name)
  (assocrv :members
           (api-request :get #?"/orgs/$(org-id)/members"
                        `(("name" . ,name)))))

(defun get-orgs ()
  (assocrv :orgs (api-request :get #?"/orgs")))

(defvar *project-id*)

(defun get-project (project-id)
  (api-request :get #?"/projects/$(project-id)"))

(defun get-project-members (project-id &optional name)
  (assocrv :members
           (api-request :get #?"/projects/$(project-id)/members"
                        `(("name" . ,name)))))

(defun get-projects ()
  (assocrv :projects (api-request :get "/projects")))

(defun alist-for-sequence (name sequence)
  (map 'list (lambda (item) (cons name item)) sequence))

(defun deftask (title &key description label-ids assignee-ids (project-id *project-id*))
  (api-request :post #?"/projects/$(project-id)/tasks"
               `(("title" . ,title)
                 ("description" . ,description)
                 ,@(alist-for-sequence "label-id" label-ids)
                 ,@(alist-for-sequence "assignee-id" assignee-ids))))

(defun get-tasks (&key query page order-by page-info (project-id *project-id*))
  (let ((response (api-request :get #?"/projects/$(project-id)/tasks"
                               `(("query" . ,query)
                                 ("page" . ,(when page (format nil "~A" page)))
                                 ("order-by" . ,(when order-by
                                                  (string-downcase order-by)))))))
    (if page-info
        response
        (assocrv :tasks response))))

(defun get-task (task-id &key (project-id *project-id*))
  (api-request :get #?"/projects/$(project-id)/tasks/$(task-id)"))

(defun close-task (task-id &key (project-id *project-id*))
 (api-request :patch #?"/projects/$(project-id)/tasks/$(task-id)"
              `(("state" . "closed"))))

(defun open-task (task-id &key (project-id *project-id*))
 (api-request :patch #?"/projects/$(project-id)/tasks/$(task-id)"
              `(("state" . "open"))))

(defun edit-task (task-id &key (project-id *project-id*) title description)
  (api-request :patch #?"/projects/$(project-id)/tasks/$(task-id)"
               `(("title" . ,title)
                 ("description" . ,description))))

(defun get-comments (task-id &key (project-id *project-id*))
  (assocrv :comments
           (api-request :get #?"/projects/$(project-id)/tasks/$(task-id)/comments")))

(defun comment (task-id body &key (project-id *project-id*))
  (api-request :post #?"/projects/$(project-id)/tasks/$(task-id)/comments"
               `(("body" . ,body))))

(defun edit-comment (task-id comment-id body &key (project-id *project-id*))
  (api-request :patch #?"/projects/$(project-id)/tasks/$(task-id)/comments/$(comment-id)"
               `(("body" . ,body))))

(defun get-labels (&key name (project-id *project-id*))
  (assocrv :labels
           (api-request :get #?"/projects/$(project-id)/labels"
                        `(("name" . ,name)))))
