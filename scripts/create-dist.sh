#!/bin/bash

OS=$TRAVIS_OS_NAME
VERSION=$(build/$OS/deftask version)

DIST_NAME=deftask-cli-$OS-v$VERSION
DIST_DIR=build/$DIST_NAME
mv build/$OS $DIST_DIR

TARBALL=$DIST_NAME.tar.gz

cp README.md $DIST_DIR/
cd $DIST_DIR
tar czvf ../$TARBALL *
cd -
tar tzvf build/$TARBALL

