#!/bin/bash

OS=$TRAVIS_OS_NAME
cd build/$OS

VERSION=$(./deftask version)
TARBALL=deftask-cli-$OS-v$VERSION.tar.gz

ln ../../README.md
tar czvf $TARBALL *
mkdir -p ../dists
cp $TARBALL ../dists/
ls ../dists/
