#!/bin/bash

if [ $# -eq 2 ]; then
  clj -M -m convert-ds-2-yolo $1 $2
else
  echo "uso: convert2yolo directorio-del-ds-quantum directorio-del-ds-yolo"
fi