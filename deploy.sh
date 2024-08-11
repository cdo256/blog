#!/bin/sh

server=algernon
port=22

./build.sh
ssh -p $port $server -- rm -rf /var/www/*
scp -r -P $port out/* $server:/var/www/
