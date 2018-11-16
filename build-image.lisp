(in-package #:cl-user)

(ql:quickload "deftask-cli")

#+sbcl
(progn
  (sb-ext:save-lisp-and-die "deftask-cli"
                            :executable t
                            :save-runtime-options t
                            :compression t
                            :toplevel #'deftask-cli:main))
