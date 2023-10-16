#!/bin/sh

server=algernon

rm -rf out/*
emacs -Q --script build.el
ssh $server -- rm -rf /var/www/*
scp -r out/* $server:/var/www/
