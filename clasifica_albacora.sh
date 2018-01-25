csv=$1
prog="stack exec -- clasificacion-exe"

function bloque {
echo
echo
echoSub -c - $1
echo
if [ "x$4" == x ]
then
  $prog  -c ALBACORA -t 2 -y $2 $3 $csv
else
  $prog  -c ALBACORA -t 2 -y $2 -y $3 $4 $csv
fi
echo
}

(
echoSub CLASIFICACIÓN ALBACORA POR CATEGORIAS
bloque "ALEVÍN MASCULINO" 2004 2005 -m
# echo "\\pagebreak"
bloque "ALEVÍN FEMENINO" 2005 2006 -w
# echo "\\pagebreak"
bloque "BENJAMÍN MASCULINO" 2006 2007 -m
# echo "\\pagebreak"
bloque "BENJAMÍN FEMENINO" 2007 2008 -w
echo "\\pagebreak"

echo
echoSub CLASIFICACION ALBACORA POR AÑOS
bloque "2004 MASCULINO" 2004 -m
bloque "2005 MASCULINO" 2005 -m
bloque "2006 MASCULINO" 2006 -m
bloque "2007 MASCULINO" 2007 -m
echo "\\pagebreak"

bloque "2005 FEMENINO" 2005 -w
bloque "2006 FEMENINO" 2006 -w
bloque "2007 FEMENINO" 2007 -w
bloque "2008 FEMENINO" 2008 -w
echo "\\pagebreak"

echo
echoSub CLASIFICACIÓN GENERAL ALBACORA

$prog -c ALBACORA -t 2 $csv
) | pandoc -t latex -o clasificacion.pdf 
