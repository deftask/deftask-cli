#!/bin/bash

set -e

if [[ "$TRAVIS_OS_NAME" == "linux" ]]; then
    export SB_DIST=sbcl-1.4.13-x86-64-linux
    export TARBALL=$SB_DIST-binary.tar.bz2
    wget http://prdownloads.sourceforge.net/sbcl/$TARBALL
    tar -xjvf $TARBALL
    cd $SB_DIST
    sudo sh install.sh
    cd -
fi

curl -O https://beta.quicklisp.org/quicklisp.lisp

sbcl --disable-debugger --load quicklisp.lisp --eval '(quicklisp-quickstart:install)' --eval '(setf ql-util::*do-not-prompt* t)' --eval '(ql:add-to-init-file)' --quit

cd ~/quicklisp/local-projects/
git clone https://github.com/chaitanyagupta/termcolor.git
