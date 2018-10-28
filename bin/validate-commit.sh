#!/bin/bash
clj -m main -c config/clean-conf.edn -o output/cosita2

retVal=$?
if [ $retVal -ne 0 ]; then
    echo "Error"
fi
