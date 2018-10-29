#!/bin/bash

git remote rm origin
cp -v bin/pre-commit .git/hooks/
cp -v bin/post-commit .git/hooks/
mkdir -p s3-sync-origin
mkdir -p dataset/train
mkdir -p dataset/val
cp -v bin/sync.sh s3-sync-origin/
