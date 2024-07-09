#!/usr/bin/env bash
set -euo pipefail

echo "==== Remove previous build artifacts ===="
rm -rf ./content
rm -rf ./public/
echo ""

echo "==== Generate Hugo markdown using ox-hugo ===="
emacs -Q --script build.el
echo ""

echo "==== Build with Hugo ===="
hugo
echo ""

echo "==== Copy .well-known for webfinger ===="
cp ./.well-known -r ./public/
echo ""
