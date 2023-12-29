#!/bin/sh

server=algernon

./build.sh
ssh $server -- rm -rf /var/www/*
scp -r out/* $server:/var/www/
