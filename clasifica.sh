csv=$1
prog="stack exec -- clasificacion-exe"

function bloque {
echo
echo
echoSub $1
echo
$prog  -t 2 -y $2 -y $3 $4 $csv
echo
}

(bloque "ALEVÍN MASCULINO" 2004 2005 -m
echo "\\pagebreak"
bloque "ALEVÍN FEMENINO" 2005 2006 -w
echo "\\pagebreak"
bloque "BENJAMÍN MASCULINO" 2006 2007 -m
echo "\\pagebreak"
bloque "BENJAMÍN FEMENINO" 2007 2008 -w
echo "\\pagebreak"
) | pandoc -t latex -o clasificacion.pdf 
