(in-package #:cl-user)

(ql:quickload "deftask-cli")

(setf deftask-cli::*default-pager* "less -FRX")

#+sbcl
(progn
  (sb-ext:disable-debugger)
  (sb-ext:save-lisp-and-die "deftask-cli"
                            :executable t
                            :save-runtime-options t
                            :compression t
                            :toplevel #'deftask-cli:main))
