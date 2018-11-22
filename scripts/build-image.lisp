(push *default-pathname-defaults* asdf:*central-registry*)
(ql:quickload "cffi-grovel") ; https://github.com/quicklisp/quicklisp-client/issues/108
(ql:quickload "deftask-cli")

(defvar *build-dir*
  (let ((os (or (deftask-cli::getenv "TRAVIS_OS_NAME")
                (string-downcase (uiop/os:operating-system)))))
    (assert os nil "No OS given")
    (ensure-directories-exist (format nil "build/~A/" os))))

(deftask-cli:build-image (make-pathname :name "deftask" :defaults *build-dir*))
