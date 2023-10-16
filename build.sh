#!/bin/sh

rm -rf out/*
emacs -Q --script build.el
