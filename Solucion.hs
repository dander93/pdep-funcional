module Solucion where

type Persona = (Int, Int, String, Int, [String])

obtenerEdad :: Persona -> Int
obtenerEdad (edad, _, _, _, _) = edad

obtenerSuenios :: Persona -> Int
obtenerSuenios (_, suenios, _, _, _) = suenios

obtenerNombre :: Persona -> String
obtenerNombre (_, _, nombre, _, _) = nombre

obtenerFelicidonios :: Persona -> Int
obtenerFelicidonios (_, _, _, felicidonios, _) = felicidonios

obtenerHabilidades :: Persona -> [String]
obtenerHabilidades (_, _, _, _, habilidades) = habilidades

felicidoniosAltos :: Int
felicidoniosAltos = 100

felicidoniosBajos :: Int
felicidoniosBajos = 50

--
-- Punto 1 - a
--

tieneFelicidoniosMedios :: Persona -> Bool
tieneFelicidoniosMedios persona = ((> felicidoniosBajos) . obtenerFelicidonios) persona && ((<= felicidoniosAltos) . obtenerFelicidonios) persona

obtenerCoeficienteSatisfaccion :: Persona -> Int
obtenerCoeficienteSatisfaccion persona
  | esMuyFeliz = felicidoniosPor (obtenerEdad persona)
  | esMedianamenteFeliz = felicidoniosPor (obtenerSuenios persona)
  | otherwise = obtenerFelicidonios persona `div` 2
  where
    esMuyFeliz = ((> felicidoniosAltos) . obtenerFelicidonios) persona
    esMedianamenteFeliz = tieneFelicidoniosMedios persona
    felicidoniosPor = (* obtenerFelicidonios persona)

--
-- Punto 1 - b
--

obtenerAmbicion :: Persona -> Int
obtenerAmbicion persona
  | esMuyAmbiciosa = sueniosPor (obtenerFelicidonios persona)
  | esMedianamenteAmbiciosa = sueniosPor (obtenerEdad persona)
  | otherwise = sueniosPor 2
  where
    esMuyAmbiciosa = ((> felicidoniosAltos) . obtenerFelicidonios) persona
    esMedianamenteAmbiciosa = tieneFelicidoniosMedios persona
    sueniosPor = (* obtenerSuenios persona)

--
-- Punto 2 - a
--

esNombreLargo :: Persona -> String
esNombreLargo persona
  | (length . obtenerNombre) persona > 10 = "Tiene un nombre largo"
  | otherwise = "No tiene un nombre largo"

--
-- Punto 2 - b
--

-- esto es más claro sin composición. El mayor problema es devolver los strings de la consigna
esSuertuda :: Persona -> String
esSuertuda persona
  | (even . ((* 3) . obtenerCoeficienteSatisfaccion)) persona = "Es suertuda"
  | otherwise = "No es suertuda"

--
-- Punto 2 - c
--

tieneNombreLindo :: Persona -> String
tieneNombreLindo persona
  | ((== 'a') . (last . obtenerNombre)) persona = "Tiene nombre lindo"
  | otherwise = "Tiene nombre comun" -- no tiene nombre lindo, pero nos censuran

--
-- Punto 3 - a
--

type Suenio = Persona -> Persona

serMasFeliz :: Int -> Suenio
serMasFeliz cuanto persona =
  ( obtenerEdad persona,
    obtenerSuenios persona,
    obtenerNombre persona,
    obtenerFelicidonios persona + cuanto,
    obtenerHabilidades persona
  )

multiplicadorRecibirseCarrera :: Int
multiplicadorRecibirseCarrera = 1000

recibirseCarreraCalcularFelicidonios :: String -> Int
recibirseCarreraCalcularFelicidonios = (* multiplicadorRecibirseCarrera) . length

-- se concatena la lista de carreras existente con una lista de una única carrera (input)
recibirseCarreraAgregarHabilidad :: String -> Suenio
recibirseCarreraAgregarHabilidad carrera persona =
  ( obtenerEdad persona,
    obtenerSuenios persona,
    obtenerNombre persona,
    obtenerFelicidonios persona,
    obtenerHabilidades persona ++ [carrera]
  )

recibirseCarrera :: String -> Suenio
recibirseCarrera carrera = recibirseCarreraAgregarHabilidad carrera . serMasFeliz (recibirseCarreraCalcularFelicidonios carrera)

cumplirAnios :: Persona -> Persona
cumplirAnios persona =
  ( obtenerEdad persona + 1,
    obtenerSuenios persona,
    obtenerNombre persona,
    obtenerFelicidonios persona,
    obtenerHabilidades persona
  )

multiplicadorFelicidoniosViajarListaCiudades :: Int
multiplicadorFelicidoniosViajarListaCiudades = 100

viajarListaCiudadesCalcularFelicidonios :: [String] -> Int
viajarListaCiudadesCalcularFelicidonios = (* multiplicadorFelicidoniosViajarListaCiudades) . length

viajarListaCiudades :: [String] -> Suenio
viajarListaCiudades listaCiudades = cumplirAnios . serMasFeliz (viajarListaCiudadesCalcularFelicidonios listaCiudades)

enamorarseOtraPersona :: Persona -> Suenio
enamorarseOtraPersona persona otraPersona = serMasFeliz (obtenerFelicidonios otraPersona) persona

queTodoSigaIgual :: Suenio
queTodoSigaIgual = id

comboPerfectoPremio :: Int
comboPerfectoPremio = 100

comboPerfectoCiudades :: [String]
comboPerfectoCiudades = ["Berazategui", "Paris"]

comboPerfectoCarrera :: String
comboPerfectoCarrera = "Medicina"

comboPerfecto :: Suenio
comboPerfecto = serMasFeliz comboPerfectoPremio . (viajarListaCiudades comboPerfectoCiudades . recibirseCarrera comboPerfectoCarrera)
