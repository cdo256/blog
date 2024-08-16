#!/usr/bin/env bash
set -euo pipefail

pkill hugo
sleep 5
./build.sh
./run-local
