#!/usr/bin/env bash
set -euo pipefail

echo "==== Generate Hugo markdown using ox-hugo ===="
emacs -Q --script build.el
echo ""

echo "==== Build with Hugo ===="
hugo --ignoreCache -D
echo ""

echo "==== SUCCESS ===="
