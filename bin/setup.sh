#!/bin/bash

git remote rm origin
cp -v bin/pre-commit .git/hooks/
cp -v bin/post-commit .git/hooks
