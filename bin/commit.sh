#!/bin/sh

clj -m main -c config/clean-conf.edn -o output/__tmp__
retVal=$?

if [ $retVal -ne 0 ]
then
    rm -rf output/__tmp__
    echo "\n\nVerifique que no existan directorios repetidos hijos de train o val ni directorios que existan tanto en train como val!"
fi

exit $retVal
