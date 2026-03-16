module Spec where
import PdePreludat
import Library
import Test.Hspec

correrTests :: IO ()
correrTests = hspec $ do

  describe "Test para saber si un auto está en buen estado" $ do
    it "Si es marca Peugeot, entonces se evalua de forma correcta" $ do
      buenEstadoDeSalud peugeot `shouldBe` False
    it "Si es marca Lamborghini con tiempo en pista menor a 100 y desgaste de chasis menor a 20, entonces se evalua de forma correcta" $ do
      buenEstadoDeSalud (lamborghini { tiempoDeCarrera = 99, desgasteChasis = 7 }) `shouldBe`  True
    it "Si es marca Fiat con tiempo en pista menor a 100 y desgaste de chasis mayor a 20, entonces se evalua de forma correcta" $ do
      buenEstadoDeSalud (fiat { tiempoDeCarrera = 99, desgasteChasis = 33 }) `shouldBe`  False
    it "Si es marca Ferrari con tiempo en pista mayor a 100, desgaste de chasis menor a 40 y desgaste de ruedas menor a 60, entonces se evalua correctamente" $ do
      buenEstadoDeSalud (ferrari { tiempoDeCarrera = 130, desgasteChasis = 30, desgasteRuedas = 50 }) `shouldBe`  True
    it "Si es marca Ferrari con tiempo en pista menor a 100, desgaste de chasis mayor a 40 y desgaste de ruedas menor a 60, entonces se evalua correctamente" $ do
      buenEstadoDeSalud (ferrari { tiempoDeCarrera = 15, desgasteChasis = 45, desgasteRuedas = 50 }) `shouldBe`  False
    it "Si es marca Ferrari con tiempo en pista mayor a 100, desgaste de chasis menor a 40 y desgaste de ruedas mayor a 60, entonces se evalua correctamente" $ do
      buenEstadoDeSalud (ferrari { tiempoDeCarrera = 150, desgasteChasis = 30, desgasteRuedas = 70 }) `shouldBe`  False

  describe "Test para saber si un auto no da más" $ do
    it "Si es marca Ferrari con desgaste de ruedas 20 y chasis 90, entonces se evalua de forma correcta" $ do
      noDaMas (ferrari { desgasteChasis = 90, desgasteRuedas = 20 }) `shouldBe`  True
    it "Si es marca Ferrari con desgaste de ruedas 90 y chasis 20, entonces se evalua de forma correcta" $ do
      noDaMas (ferrari { desgasteChasis = 20, desgasteRuedas = 90 }) `shouldBe`  False
    it "Si es marca Lamborghini con desgaste de ruedas 90 y chasis 20, entonces se evalua correctamente" $ do
      noDaMas (lamborghini { desgasteChasis = 20, desgasteRuedas = 90 }) `shouldBe`  True
    it "Si es marca Lamborghini, entonces se evalua correctamente" $ do
      noDaMas lamborghini `shouldBe` False

  describe "Test para saber si un auto es un chiche" $ do
    it "Un auto de marca Lamborghini es un chiche" $ do
      esUnChiche lamborghini `shouldBe` True
    it "Un auto de marca Lamborghini, con desgaste de ruedas 90 y chasis 20, no es un chiche" $ do
      esUnChiche (lamborghini { desgasteRuedas = 90, desgasteChasis = 20 }) `shouldBe` False
    it "Un auto de marca Peugeot es un chiche" $ do
      esUnChiche peugeot `shouldBe` True
    it "Un auto de marca Ferrari no es un chiche" $ do
      esUnChiche ferrari `shouldBe` False

  describe "Tests para esUnaJoya" $ do
    it "Auto sin desgaste y con un solo apodo es una joya" $ do
      esUnaJoya peugeot `shouldBe` True
    it "Auto sin desgaste pero con más de un apodo no es una joya" $ do
      esUnaJoya (peugeot { apodos = ["El rey del desierto", "El fierro"] }) `shouldBe` False
    it "Auto con desgaste y con marca de más de 5 caracteres es una joya" $ do
      esUnaJoya lamborghini `shouldBe` True
    it "Auto con desgaste y marca de menos de 6 caracteres no es una joya" $ do
      esUnaJoya fiat `shouldBe` False

  describe "Test para reparar un auto" $ do
    it "Si es marca Fiat, entonces se evalua de forma correcta" $ do
      (desgasteChasis . repararAuto) fiat `shouldBe` 4.95
      (desgasteRuedas . repararAuto) fiat `shouldBe` 0
    it "Si es marca Ferrari, entonces se evalua correctamente" $ do
      (desgasteChasis . repararAuto) ferrari `shouldBe` 0
      (desgasteRuedas . repararAuto) ferrari `shouldBe` 0

  describe "Test para penalizar con X segundos a un auto" $ do
    it "Si es marca Ferrari con tiempo 10 segundos en pista y se le aplica una penalidad de 20 segundos, entonces se evalua de forma correcta" $ do
      (tiempoDeCarrera . penalizarAuto 20) (ferrari { tiempoDeCarrera = 10 }) `shouldBe` 30
    it "Si es marca Ferrari con tiempo 10 segundos en pista y se le aplica una penalidad de 0 segundos, entonces se evalua correctamente" $ do
      (tiempoDeCarrera . penalizarAuto 0) (ferrari { tiempoDeCarrera = 10 }) `shouldBe` 10

  describe "Test para ponerle nitro a un auto" $ do
    it "Si le ponemos nitro a un fiat, su velocidad máxima aumenta a 52,8 m/s" $ do
      (velocidadMaxima . ponerNitroAlAuto) fiat `shouldBe` 52.8
    it "Si le ponemos nitro a un fiat, cuya velocidad máxima es 0 m/s, su velocidad máxima se mantendrá igual" $ do
      (velocidadMaxima . ponerNitroAlAuto) (fiat { velocidadMaxima = 0 }) `shouldBe` 0

  describe "Tests para bautizarAuto" $ do
    it "Agregar otro apodo más luego de uno anterior" $ do
      (apodos . bautizarAuto "El diablo") lamborghini `shouldBe` ["Lambo", "La bestia", "El diablo"]
    it "Agregar un nuevo apodo al final de la lista" $ do
      (apodos . bautizarAuto "El diablo") (lamborghini { apodos = [] }) `shouldBe` ["El diablo"]

  describe "Test para transitar un tramo curvo con un auto" $ do
    it "Transitar una curva peligrosa con un auto marca Ferrari deja el desgaste de ruedas en 15" $ do
      (desgasteRuedas . curvaPeligrosa) ferrari `shouldBe` 15
    it "Transitar una curva peligrosa con un auto marca Ferrari mantiene el desgaste del chasis en 0" $ do
      (desgasteChasis . curvaPeligrosa) ferrari `shouldBe` 0
    it "Transitar una curva peligrosa con un auto marca Ferrari aumenta el tiempo de carrera a 23.5 seg." $ do
      (tiempoDeCarrera . curvaPeligrosa) ferrari `shouldBe` 23.5
    it "Transitar una curva tranca con un auto marca Ferrari deja el desgaste de ruedas en 15" $ do
      (desgasteRuedas . curvaTranca) ferrari `shouldBe` 15
    it "Transitar una curva tranca con un auto marca Ferrari mantiene el desgaste del chasis en 0" $ do
      (desgasteChasis . curvaTranca) ferrari `shouldBe` 0
    it "Transitar una curva tranca con un auto marca Ferrari aumenta el tiempo de carrera a 48.5 seg." $ do
      (tiempoDeCarrera . curvaTranca) ferrari `shouldBe` 48.5

  describe "Test para transitar un tramo recto con un auto" $ do
    it "Transitar un tramo recto classic con un auto marca Ferrari deja el desgaste del chasis en 7.15" $ do
      (desgasteChasis . tramoRectoClassic) ferrari `shouldBe` 7.15
    it "Transitar un tramo recto classic con un auto marca Ferrari aumenta el tiempo de carrera a 11 seg." $ do
      (tiempoDeCarrera . tramoRectoClassic) ferrari `shouldBe` 11
    it "Transitar un tramito con un auto marca Ferrari deja el desgaste del chasis en 2.6" $ do
      (desgasteChasis . tramito) ferrari `shouldBe` 2.6
    it "Transitar un tramito con un auto marca Ferrari aumenta el tiempo de carrera a 4 seg." $ do
      (tiempoDeCarrera . tramito) ferrari `shouldBe` 4

  describe "Test para transitar un tramo en zigzag con un auto" $ do
    it "Transitar un tramo zigzag loco con un auto marca Ferrari tiene un desgaste de chasis de 5" $ do
      (desgasteChasis . zigZagLoco) ferrari `shouldBe` 5
    it "Transitar un tramo zigzag loco con un auto marca Ferrari tiene un desgaste de ruedas de 32.5" $ do
      (desgasteRuedas . zigZagLoco) ferrari `shouldBe` 32.5
    it "Transitar un tramo zigzag loco con un auto marca Ferrari aumenta el tiempo de pista a 15 seg." $ do
      (tiempoDeCarrera . zigZagLoco) ferrari `shouldBe` 15
    it "Transitar un tramo casi curva con un auto marca Ferrari tiene un desgaste de chasis de 5" $ do
      (desgasteChasis . casiCurva) ferrari `shouldBe` 5
    it "Transitar un tramo casi curva con un auto marca Ferrari tiene un desgaste de ruedas de 6.5" $ do
      (desgasteRuedas . casiCurva) ferrari `shouldBe` 6.5
    it "Transitar un tramo casi curva con un auto marca Ferrari aumenta el tiempo de pista a 3 seg." $ do
      (tiempoDeCarrera . casiCurva) ferrari `shouldBe` 3

  describe "Test para transitar un tramo rulo en el aire con un auto" $ do
    it "Transitar un tramo ruloClasico con un auto marca Ferrari tiene un desgaste de chasis de 0" $ do
      (desgasteChasis . ruloClasico) ferrari `shouldBe` 0
    it "Transitar un tramo ruloClasico con un auto marca Ferrari tiene un desgaste de ruedas de 19.5" $ do
      (desgasteRuedas . ruloClasico) ferrari `shouldBe` 19.5
    it "Transitar un tramo ruloClasico con un auto marca Ferrari aumenta el tiempo de pista a 1 seg." $ do
      (tiempoDeCarrera . ruloClasico) ferrari `shouldBe` 1
    it "Transitar un tramo deseoDeMuerte con un auto marca Ferrari tiene un desgaste de chasis de 0" $ do
      (desgasteChasis . deseoDeMuerte) ferrari `shouldBe` 0
    it "Transitar un tramo deseoDeMuerte con un auto marca Ferrari tiene un desgaste de ruedas de 39.0" $ do
      (desgasteRuedas . deseoDeMuerte) ferrari `shouldBe` 39.0
    it "Transitar un tramo deseoDeMuerte con un auto marca Ferrari aumenta el tiempo de pista a 2 seg." $ do
      (tiempoDeCarrera . deseoDeMuerte) ferrari `shouldBe` 2
  
  describe "Tests para arrancanONoArrancan" $ do
    it "Para una lista de autos ferrari, lamborghini, fiat, peugeot, entonces se evalua de forma correcta" $ do
      arrancanONoArrancan [ferrari, lamborghini, fiat, peugeot] `shouldBe` ["600", "504"]
    it "Para una lista de autos ferrari, lamborghini, fiat, peugeot, entonces se evalua de forma correcta" $ do
      arrancanONoArrancan [ferrari, lamborghini] `shouldBe` []
  
  describe "Tests para grupoAltoEmbole" $ do
    it "Para un lamborghini y una ferrari de desgaste de ruedas 20 y chasis 90 pedimos un grupo alto embole de los dos primeros, entonces se evalua de forma correcta" $ do
      grupoAltoEmbole 2 [lamborghini, ferrari { desgasteChasis = 90, desgasteRuedas = 20 }] `shouldBe` True
    it "Para un lamborghini, una ferrari y un fiat pedimos un grupo alto embole de los tres primeros, entonces se evalua de forma correcta" $ do
      grupoAltoEmbole 3 [lamborghini, ferrari, fiat] `shouldBe` False
    it "Para un grupo sin autos pedimos un grupo alto embole de los 5 primeros, entonces se evalua de forma correcta" $ do
      grupoAltoEmbole 5 [] `shouldBe` False

  describe "Tests para clasicoDeClasicos" $ do
    it "Para un Lamborghini de 30 seg. en carrera y una Ferrari de 10 seg. en carrera, con un desgaste de ruedas de 20 y un desgaste de chasis de 90, satisface" $ do
      clasicoDeClasicos 20 [lamborghini { tiempoDeCarrera = 30 }, ferrari { tiempoDeCarrera = 10, desgasteRuedas = 20, desgasteChasis = 90}] `shouldBe` True
    it "Para un Lamborghini de 30 seg. en carrera y una Ferrari de 30 seg. en carrera, con un desgaste de ruedas de 20 y un desgaste de chasis de 90, satisface" $ do
      clasicoDeClasicos 20 [lamborghini { tiempoDeCarrera = 30 }, ferrari { tiempoDeCarrera = 30, desgasteRuedas = 20, desgasteChasis = 90}] `shouldBe` False

  describe "Tests para nivelDeAutosClavo" $ do
    it "Para un Fiat y un Lamborghini, y un umbral de 30, satisface (33+7 = 40 > 30)" $ do
      nivelDeAutosClavo 30 [fiat, lamborghini] `shouldBe` True
    it "Para un Fiat y un Lamborghini, y un umbral de 40, no satisface (33+7 = 40 == 40)" $ do
      nivelDeAutosClavo 40 [fiat, lamborghini] `shouldBe` False
    it "Para un Fiat y un Lamborghini, y un umbral de 45, no satisface (33+7 = 40 < 45)" $ do
      nivelDeAutosClavo 45 [fiat, lamborghini] `shouldBe` False

  describe "Tests para estanOrdenados" $ do
    it "Para una Ferrari con tiempo en carrera 10, otra Ferrari con tiempo en carrera 20 y otra Ferrari con tiempo en carrera 30 para el criterio tiempo en carrera de un auto <= tiempo en carrera siguiente, entonces se evalua correctamente" $ do
      estanOrdenados "ascendente" tiempoDeCarrera [ferrari { tiempoDeCarrera = 10 }, ferrari { tiempoDeCarrera = 20 }, ferrari { tiempoDeCarrera = 30 }] `shouldBe` True
    it "Para un Lamborghini y una Ferrari para el criterio una marca <= a la siguiente marca, entonces se evalua correctamente" $ do
      estanOrdenados "ascendente" marca [lamborghini, ferrari] `shouldBe` False
    it "Para un Lamborghini para el criterio una marca <= a la siguiente marca, entonces se evalua correctamente" $ do
      estanOrdenados "ascendente" marca [lamborghini] `shouldBe` True
    it "Para un grupo sin autos, entonces se evalua correctamente" $ do
      estanOrdenados "descendente" tiempoDeCarrera [] `shouldBe` True

  describe "Tests para altoToc" $ do
    it "Para el grupo: Lamborghini, Ferrari, Fiat y Peugeot (en ese orden); si el criterio es que no tenga desgaste en el chasis, satisface" $ do
      altoToc ((==) 0 . desgasteChasis) [lamborghini, ferrari, fiat, peugeot] `shouldBe` True
    it "Para el grupo: Lamborghini, Fiat (en ese orden); si el criterio es que no tenga desgaste en el chasis, no satisface" $ do
      altoToc ((==) 0 . desgasteChasis) [lamborghini, fiat] `shouldBe` False
    it "Para el grupo: Lamborghini (en ese orden), si el criterio es que no tenga desgaste en el chasis, satisface" $ do
      altoToc ((==) 0 . desgasteChasis) [lamborghini] `shouldBe` True
    it "Para un grupo sin autos, si el criterio es que no tenga desgaste en el chasis, se satisface" $ do
      altoToc ((==) 0 . desgasteChasis) [] `shouldBe` True
  
  describe "Tests para elMasGroso" $ do
    it "Para un Lamborghini con 10s y una Ferrari con 20s, con criterio mayor tiempo en pista, gana Ferrari" $ do
      modelo (elMasGroso tiempoDeCarrera [lamborghini { tiempoDeCarrera = 10 }, ferrari { tiempoDeCarrera = 20 }]) `shouldBe` "F50"
    it "Para una Ferrari y un Lamborghini, con criterio cantidad de apodos, gana Ferrari" $ do
      marca (elMasGroso (length . apodos) [ferrari, lamborghini]) `shouldBe` "Ferrari"

  describe "Test para saber cómo queda un auto después de recorrer una pista" $ do
    it "Si un Ferrari corre en Monza, queda con 102 seg. de tiempo en pista" $ do
      (tiempoDeCarrera . correrPista monza) ferrari `shouldBe` 102
    it "Si un Ferrari corre en Monza, queda con un desgaste de ruedas de 62.5" $ do
      (desgasteRuedas . correrPista monza) ferrari `shouldBe` 62.5
    it "Si un Ferrari corre en Monza, queda con un desgaste de chasis de 14.75" $ do
      (desgasteChasis . correrPista monza) ferrari `shouldBe` 14.75