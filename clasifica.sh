csv=$1
prog="stack exec -- clasificacion"

function bloque {
echo
echo
echoSub $1 "($2, $3)"
echo
$prog  -t 2 -y $2 -y $3 $4 $csv
echo
}

(bloque "ALEVÍN MASCULINO" 2005 2006 -m
echo "\\pagebreak"
bloque "ALEVÍN FEMENINO" 2006 2007 -w
echo "\\pagebreak"
bloque "BENJAMÍN MASCULINO" 2007 2008 -m
echo "\\pagebreak"
bloque "BENJAMÍN FEMENINO" 2008 2009 -w
echo "\\pagebreak"
) | pandoc -t latex -o clasificacion.pdf 
