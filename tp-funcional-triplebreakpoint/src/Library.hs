{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Eta reduce" #-}
module Library where
import PdePreludat

-- Primera Entrega

data Auto = Auto {
  marca :: String,
  modelo :: String,
  desgasteChasis :: Number,
  desgasteRuedas :: Number,
  velocidadMaxima :: Number,
  tiempoDeCarrera :: Number,
  apodos :: [String]
} deriving(Show)

ferrari :: Auto
ferrari = Auto {
  marca = "Ferrari",
  modelo = "F50",
  desgasteChasis = 0,
  desgasteRuedas = 0,
  velocidadMaxima = 65,
  tiempoDeCarrera = 0,
  apodos = ["La nave", "El fierro", "Ferrucho"]
}

lamborghini :: Auto
lamborghini = Auto {
  marca = "Lamborghini",
  modelo = "Diablo",
  desgasteChasis = 7,
  desgasteRuedas = 4,
  velocidadMaxima = 73,
  tiempoDeCarrera = 0,
  apodos = ["Lambo", "La bestia"]
}

fiat :: Auto
fiat = Auto {
  marca = "Fiat",
  modelo = "600",
  desgasteChasis = 33,
  desgasteRuedas = 27,
  velocidadMaxima = 44,
  tiempoDeCarrera = 0,
  apodos = ["La Bocha", "La bolita", "Fitito"]
}

peugeot :: Auto
peugeot = Auto {
  marca = "Peugeot",
  modelo = "504",
  desgasteChasis = 0,
  desgasteRuedas = 0,
  velocidadMaxima = 40,
  tiempoDeCarrera = 0,
  apodos = ["El rey del desierto"]
}

buenEstadoDeSalud :: Auto -> Bool
buenEstadoDeSalud auto
  | ((==) "Peugeot" . marca) auto = False
  | ((< 100) . tiempoDeCarrera) auto = ((< 20) . desgasteChasis) auto
  | otherwise = ((< 40) . desgasteChasis) auto && ((< 60) . desgasteRuedas) auto

noDaMas :: Auto -> Bool
noDaMas auto
  | ((==) "La " . take 3 . head . apodos) auto = ((> 80) . desgasteChasis) auto
  | otherwise = ((> 80) . desgasteRuedas) auto

esUnChiche :: Auto -> Bool
esUnChiche auto
  | (even . length . apodos) auto = ((< 20) . desgasteChasis) auto
  | otherwise = (even . velocidadMaxima) auto

-- Función auxiliar para esUnaJoya
tieneDesgaste :: Auto -> Bool
tieneDesgaste auto = ((> 0) . desgasteChasis) auto || ((> 0) . desgasteRuedas) auto

esUnaJoya :: Auto -> Bool
esUnaJoya auto = (not . tieneDesgaste) auto && ((==) 1 . length . apodos) auto || tieneDesgaste auto && ((> 5) . length . marca) auto

repararAuto :: Auto -> Auto
repararAuto auto = auto {
  desgasteChasis = ((*) 0.15 . desgasteChasis) auto,
  desgasteRuedas = 0
}

penalizarAuto :: Number -> Auto -> Auto
penalizarAuto segundos auto = auto {
  tiempoDeCarrera = ((+) segundos . tiempoDeCarrera) auto
}

ponerNitroAlAuto :: Auto -> Auto
ponerNitroAlAuto auto = auto { 
  velocidadMaxima = ((*) 1.2 . velocidadMaxima) auto 
}

bautizarAuto :: String -> Auto -> Auto
bautizarAuto nuevoApodo auto = auto { 
  apodos = ((++ [nuevoApodo]) . apodos) auto 
}

data Pista = Pista {
  nombre :: String,
  pais :: String,
  precioBaseEntrada :: Number,
  tramos :: [Tramo]
} deriving(Show)

type Tramo = Auto -> Auto

-- Funciones auxiliares para evitar la duplicación de código en cada tramo
sumarTiempoDeCarrera :: Number -> Auto -> Auto
sumarTiempoDeCarrera tiempoExtra auto = auto {
  tiempoDeCarrera = ((+) tiempoExtra . tiempoDeCarrera) auto
}

sumarDesgasteRuedas :: Number -> Auto -> Auto
sumarDesgasteRuedas desgasteExtra auto = auto {
  desgasteRuedas = ((+) desgasteExtra . desgasteRuedas) auto
}

sumarDesgasteChasis :: Number -> Auto -> Auto
sumarDesgasteChasis desgasteExtra auto = auto {
  desgasteChasis = ((+) desgasteExtra . desgasteChasis) auto
}

transitarCurva :: Number -> Number -> Tramo
transitarCurva angulo longitud auto = (sumarDesgasteRuedas (3 * longitud / angulo) . sumarTiempoDeCarrera ((longitud - velocidadMaxima auto) / 10)) auto

curvaPeligrosa :: Tramo
curvaPeligrosa = transitarCurva 60 300

curvaTranca :: Tramo
curvaTranca = transitarCurva 110 550

transitarRecta :: Number -> Tramo
transitarRecta longitud auto = (sumarDesgasteChasis (longitud * 0.01) . sumarTiempoDeCarrera (longitud / velocidadMaxima auto)) auto

tramoRectoClassic :: Tramo
tramoRectoClassic = transitarRecta 715

tramito :: Tramo
tramito = transitarRecta 260

transitarZigZag :: Number -> Tramo
transitarZigZag cambiosDeDireccion auto = (sumarTiempoDeCarrera (cambiosDeDireccion * 3) . sumarDesgasteChasis 5 . sumarDesgasteRuedas (velocidadMaxima auto * cambiosDeDireccion / 10)) auto

zigZagLoco :: Tramo
zigZagLoco = transitarZigZag 5

casiCurva :: Tramo
casiCurva = transitarZigZag 1

transitarRuloEnElAire :: Number -> Tramo
transitarRuloEnElAire diametro auto = (sumarDesgasteRuedas (diametro * 1.5) . sumarTiempoDeCarrera (5 * diametro / velocidadMaxima auto)) auto

ruloClasico :: Tramo
ruloClasico = transitarRuloEnElAire 13

deseoDeMuerte :: Tramo
deseoDeMuerte = transitarRuloEnElAire 26

-- Segunda Entrega

arrancanONoArrancan :: [Auto] -> [String]
arrancanONoArrancan autos = (map modelo . filter (not . buenEstadoDeSalud)) autos

grupoAltoEmbole :: Number -> [Auto] -> Bool
grupoAltoEmbole primerosAutos autos = (any noDaMas . take primerosAutos) autos

clasicoDeClasicos :: Number -> [Auto] -> Bool
clasicoDeClasicos tiempo autos = (all esUnChiche . filter ((> tiempo) . tiempoDeCarrera)) autos

nivelDeAutosClavo :: Number -> [Auto] -> Bool
nivelDeAutosClavo umbral autos = ((> umbral) . sumOf desgasteChasis) autos

estanOrdenados :: Ord a => String -> (Auto -> a) -> [Auto] -> Bool
estanOrdenados _ _ [] = True
estanOrdenados _ _ [_] = True
estanOrdenados "ascendente" criterio (unAuto : otroAuto : listaDeAutos) = criterio unAuto <= criterio otroAuto && estanOrdenados "ascendente" criterio (otroAuto : listaDeAutos)
estanOrdenados "descendente" criterio (unAuto : otroAuto : listaDeAutos) = criterio unAuto >= criterio otroAuto && estanOrdenados "descendente" criterio (otroAuto : listaDeAutos)

altoToc :: (Auto -> Bool) -> [Auto] -> Bool
altoToc _ [] = True
altoToc _ [_] = True
altoToc criterio (autoPosImpar : autoPosPar : autosRestantes) = criterio autoPosPar && altoToc criterio autosRestantes

elMasGroso :: (Auto -> Number) -> [Auto] -> Auto
elMasGroso _ [unAuto] = unAuto
elMasGroso criterio (unAuto : otroAuto : listaDeAutos)
  | criterio unAuto >= criterio otroAuto = elMasGroso criterio (unAuto : listaDeAutos)
  | otherwise = elMasGroso criterio (otroAuto : listaDeAutos)

monza :: Pista
monza = Pista {
  nombre = "Monza",
  pais = "Italia",
  precioBaseEntrada = 33,
  tramos = [curvaTranca, tramoRectoClassic, zigZagLoco, curvaPeligrosa, tramito]
}

correrPista :: Pista -> Auto -> Auto
correrPista pista auto = foldr ($) auto (tramos pista)

{- 
3a: Si la lista de tramos es infinita el algoritmo diverge, debido a que no es posible acotar la ejecución de "foldr". La
única forma de reducir la expresión es que todos los tramos de la pista se apliquen al auto pasado como semilla, cosa que
no es posible dado que tenemos una lista infinita.

3b: Si le pasamos una lista infinita de autos a grupoAltoEmbole, la función toma solo los primeros
"n" autos de la lista aunque esta sea infinita, por lo que la funcion termina y devuelve un resultado correctamente
ya que nunca evalúa toda la lista infinita sino que solamente los primeros n elementos que a nosotros nos interesan.

3c: Si a la función "clasicoDeClasicos" le pasamos una lista infinita de autos, tenemos 2 posibles caminos de ejecución:
 
  + La función se ejecuta hasta que encuentre un auto que no es un chiche. Esto sucede porque la función "all" verifica que
  TODOS los elemenos de la lista filtrada sean un chiche, por lo que basta con que uno sólo no lo sea para que no se cumpla.
  Entonces, el algoritmo sí es capaz de converger a un valor (False).

  + La función se ejecuta indefinidamente porque no encuentra ningún auto que no sea un chiche. En este caso, no es posible
  acotar la ejecución de ninguna de las 2 funciones que componen a "clasicoDeClasicos". Por un lado, "filter" intentará
  filtrar una lista infinita, generando otra lista potencialmente infinita. Mientras que "all" verificará que cada uno de
  los autos, de esta última lista, sean chiches. Como todos cumplen, esta verificación continúa indefinidamente, por lo que
  no es posible reducir la expresión (el algoritmo diverge).

3d: Si le pasamos una lista infinita a la función nivelDeAutosClavo, el algoritmo diverge.
Esto pasa porque usa la función sum, que necesita recorrer toda la lista de autos para acumular los valores de desgaste de chasis.
Como la lista no tiene fin, la evaluación no puede reducirse completamente, y por lo tanto nunca termina. 
-}