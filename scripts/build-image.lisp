(push *default-pathname-defaults* asdf:*central-registry*)
(ql:quickload "cl-fad")
(ql:quickload "deftask-cli")

(defvar *build-dir*
  (let ((os (deftask-cli::getenv "TRAVIS_OS_NAME")))
    (assert os nil "No OS given")
    (ensure-directories-exist (format nil "build/~A/" os))))

(cl-fad:copy-file "README.md" (make-pathname :name "README.md" :defaults *build-dir*) :overwrite t)

(deftask-cli:build-image (make-pathname :name "deftask" :defaults *build-dir*))
