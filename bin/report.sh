#!/bin/bash
cd $1
cat cls2id.edn
echo "Numero total de etiquetas train"
cat labels-train.csv | wc -l
echo ""
echo "Etiquetas train clase 1,2,3,4,5"
cat labels-train.csv | egrep ",1$" | wc -l
cat labels-train.csv | egrep ",2$" | wc -l
cat labels-train.csv | egrep ",3$" | wc -l
cat labels-train.csv | egrep ",4$" | wc -l
cat labels-train.csv | egrep ",5$" | wc -l

echo "Numero total de etiquetas val"
cat labels-val.csv | wc -l
echo ""
echo "Etiquetas val clase 1,2,3,4,5"
cat labels-val.csv | egrep ",1$" | wc -l
cat labels-val.csv | egrep ",2$" | wc -l
cat labels-val.csv | egrep ",3$" | wc -l
cat labels-val.csv | egrep ",4$" | wc -l
cat labels-val.csv | egrep ",5$" | wc -l
