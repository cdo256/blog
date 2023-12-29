#!/bin/sh

rm -rf out/*
emacs -Q --script build.el
cp out/octocurious/pages/index.html out/octocurious/index.html
#ln -s out/mutix/pages/index.html out/mutix/index.html
