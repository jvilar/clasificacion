csv=$1
prog="stack exec -- clasificacion-exe"

function bloque {
echo
echo
echoSub $1
echo
$prog -y $2 -y $3 $4 $csv
echo
}

bloque "ALEVIN MASCULINO" 2004 2005 -m
bloque "ALEVIN FEMENINO" 2005 2006 -w
bloque "BENJAMÍN MASCULINO" 2006 2007 -m
bloque "BENJAMÍN FEMENINO" 2007 2008 -w
