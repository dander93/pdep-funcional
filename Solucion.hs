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

obtenerCoeficienteSatisfaccion :: Persona -> Int
obtenerCoeficienteSatisfaccion persona
  | obtenerFelicidonios persona > felicidoniosAltos = obtenerFelicidonios persona * obtenerEdad persona
  | obtenerFelicidonios persona <= felicidoniosAltos && obtenerFelicidonios persona > felicidoniosBajos = obtenerSuenios persona * obtenerFelicidonios persona
  | otherwise = obtenerFelicidonios persona `div` 2

--
-- Punto 1 - b
--

obtenerAmbicion :: Persona -> Int
obtenerAmbicion persona
  | obtenerFelicidonios persona > felicidoniosAltos = obtenerFelicidonios persona * obtenerSuenios persona
  | obtenerFelicidonios persona <= felicidoniosAltos && obtenerFelicidonios persona > felicidoniosBajos = obtenerEdad persona * obtenerSuenios persona
  | otherwise = obtenerSuenios persona * 2

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

serMasFeliz :: Int -> Persona -> Persona
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
recibirseCarreraAgregarHabilidad :: String -> Persona -> Persona
recibirseCarreraAgregarHabilidad carrera persona =
  ( obtenerEdad persona,
    obtenerSuenios persona,
    obtenerNombre persona,
    obtenerFelicidonios persona,
    obtenerHabilidades persona ++ [carrera]
  )

recibirseCarrera :: String -> Persona -> Persona
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

viajarListaCiudades :: [String] -> Persona -> Persona
viajarListaCiudades listaCiudades = cumplirAnios . serMasFeliz (viajarListaCiudadesCalcularFelicidonios listaCiudades)

enamorarseOtraPersona :: Persona -> Persona -> Persona
enamorarseOtraPersona persona otraPersona = serMasFeliz (obtenerFelicidonios otraPersona) persona

queTodoSigaIgual :: Persona -> Persona
queTodoSigaIgual persona = persona

comboPerfectoPremio :: Int
comboPerfectoPremio = 100

comboPerfectoCiudades :: [String]
comboPerfectoCiudades = ["Berazategui", "Paris"]

comboPerfectoCarrera :: String
comboPerfectoCarrera = "Medicina"

comboPerfecto :: Persona -> Persona
comboPerfecto = serMasFeliz comboPerfectoPremio . (viajarListaCiudades comboPerfectoCiudades . recibirseCarrera comboPerfectoCarrera)
