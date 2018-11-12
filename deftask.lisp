(defpackage #:deftask
  (:use #:cl)
  (:export #:*endpoint* #:*token* #:*version*
           #:current-user #:get-user
           #:get-org #:get-org-members #:get-orgs
           #:*project-id* #:get-project #:get-projects
           #:deftask #:list-tasks #:get-task #:edit-task
           #:comment #:edit-comment))

(in-package #:deftask)

(cl-interpol:enable-interpol-syntax)

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

;;; sdk

(defvar *endpoint* "http://api.deftask-local.com:7680")

(defvar *token* nil)

(defvar *version* "0.1")

(defun api-request (method path &optional parameters)
  (let ((url (quri:render-uri
              (quri:merge-uris (quri:make-uri :query `(("v" . ,*version*))
                                              :defaults (quri:uri path))
                               (quri:uri *endpoint*))))
        (drakma:*text-content-types* (cons '("application" . "json") drakma:*text-content-types*))
        (content (when (member method '(:post :patch))
                   (quri:url-encode-params (remove nil parameters :key #'cdr)))))
    (multiple-value-bind (body status headers)
        (drakma:http-request url
                             :method method
                             :basic-authorization (list "bearer" *token*)
                             :content content)
      (if (and (>= status 200) (< status 300))
          (let ((content-type (cdr (assoc :content-type headers))))
            (when (alexandria:starts-with-subseq "application/json" content-type)
              (json:decode-json-from-source body)))
          (error "Bad status code: ~A" status)))))

(defun current-user ()
  (api-request :get "/user"))

(defun get-user (user-id)
  (api-request :get #?"/users/$(user-id)"))

(defun get-org (org-id)
  (api-request :get #?"/orgs/$(org-id)"))

(defun get-org-members (org-id)
  (api-request :get #?"/orgs/$(org-id)/members"))

(defun get-orgs ()
  (api-request :get #?"/orgs"))

(defvar *project-id*)

(defun get-project (project-id)
  (api-request :get #?"/projects/$(project-id)"))

(defun get-projects ()
  (assocrv :projects (api-request :get "/projects")))

(defun deftask (title &key description (project-id *project-id*))
  (api-request :post #?"/projects/$(project-id)/tasks"
               `(("title" . ,title)
                 ("description" . ,description))))

(defun list-tasks (&key query (page 1) order-by (project-id *project-id*))
  (assocrv :tasks
           (api-request :get #?"/projects/$(project-id)/tasks"
                        `(("query" . ,query)
                          ("page" . ,(format nil "~A" page))
                          ("order-by" . ,(when order-by
                                           (string-downcase order-by)))))))

(defun get-task (task-id &key (project-id *project-id*))
  (api-request :get #?"/projects/$(project-id)/tasks/$(task-id)"))

(defun edit-task (task-id &key (project-id *project-id*) title description)
  (api-request :patch #?"/projects/$(project-id)/tasks/$(task-id)"
               `(("title" . ,title)
                 ("description" . ,description))))

(defun comment (task-id body &key (project-id *project-id*))
  (api-request :post #?"/projects/$(project-id)/tasks/$(task-id)/comments"
               `(("body" . ,body))))

(defun edit-comment (task-id comment-id body &key (project-id *project-id*))
  (api-request :patch #?"/projects/$(project-id)/tasks/$(task-id)/comments/$(comment-id)"
               `(("body" . ,body))))
