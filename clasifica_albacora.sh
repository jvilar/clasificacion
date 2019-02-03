csv=$1
prog="stack exec -- clasificacion"

function bloque {
echo
echo
echoSub -c - $1
echo
if [ "x$4" == x ]
then
  $prog  -c "C.N. Albacora" -t 2 -y $2 $3 $csv
else
  $prog  -c "C.N. Albacora" -t 2 -y $2 -y $3 $4 $csv
fi
echo
}

(
echoSub CLASIFICACIÓN ALBACORA POR CATEGORIAS
bloque "ALEVÍN MASCULINO" 2005 2006 -m
# echo "\\pagebreak"
bloque "ALEVÍN FEMENINO" 2006 2007 -w
# echo "\\pagebreak"
bloque "BENJAMÍN MASCULINO" 2007 2008 -m
# echo "\\pagebreak"
bloque "BENJAMÍN FEMENINO" 2008 2009 -w
echo "\\pagebreak"

echo
echoSub CLASIFICACION ALBACORA POR AÑOS
bloque "2005 MASCULINO" 2005 -m
bloque "2006 MASCULINO" 2006 -m
bloque "2007 MASCULINO" 2007 -m
bloque "2008 MASCULINO" 2008 -m
echo "\\pagebreak"

bloque "2006 FEMENINO" 2006 -w
bloque "2007 FEMENINO" 2007 -w
bloque "2008 FEMENINO" 2008 -w
bloque "2009 FEMENINO" 2009 -w
echo "\\pagebreak"

echo
echoSub CLASIFICACIÓN GENERAL ALBACORA

$prog -c "C.N. Albacora" -t 2 $csv
) | pandoc -t latex -o clasificacion.pdf 
