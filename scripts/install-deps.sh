#!/bin/bash

curl -O https://beta.quicklisp.org/quicklisp.lisp

sbcl --disable-debugger --load quicklisp.lisp --eval '(quicklisp-quickstart:install)' --eval '(setf ql-util::*do-not-prompt* t)' --eval '(ql:add-to-init-file)' --quit

cd ~/quicklisp/local-projects/
git clone https://github.com/chaitanyagupta/termcolor.git
