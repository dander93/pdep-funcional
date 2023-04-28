import Solucion

-- Para ejecutar hay que llamar a la función main de este archivo con ghci
-- ghci pruebas
-- main

persona1 :: Persona
persona1 = (25, 2, "Evangelina", 101, ["Programación", "Diseño"])

persona2 :: Persona
persona2 = (26, 2, "Maximiliano", 100, ["Ingeniería", "Matemática"])

persona3 :: Persona
persona3 = (1, 1, "Ariel", 14, ["dormir", "dibujar", "tocar la guitarra"])

persona4 :: Persona
persona4 = (15, 30, "Melina", 12, ["dormir", "dibujar"])

persona5 :: Persona
persona5 = (28, 35, "Daniel", 250, ["dormir", "dibujar"])

ejecutarYmostrarResultado fn = show . fn

mostrarTextoYresultado texto fn param = print (texto ++ ejecutarYmostrarResultado fn param)

main :: IO ()
main = do
  ---------------------------------------------------------
  -- 1 a
  ---------------------------------------------------------

  mostrarTextoYresultado "Ejercicio 1.a.1: " obtenerCoeficienteSatisfaccion persona1
  mostrarTextoYresultado "Ejercicio 1.a.2: " obtenerCoeficienteSatisfaccion persona2
  mostrarTextoYresultado "Ejercicio 1.a.3: " obtenerCoeficienteSatisfaccion persona3

  ---------------------------------------------------------
  -- 1 b
  ---------------------------------------------------------
  mostrarTextoYresultado "Ejercicio 1.b.1 " obtenerAmbicion persona1
  mostrarTextoYresultado "Ejercicio 1.b.2 " obtenerAmbicion persona2
  mostrarTextoYresultado "Ejercicio 1.b.3 " obtenerAmbicion persona3

  ---------------------------------------------------------
  -- 2 a
  ---------------------------------------------------------

  mostrarTextoYresultado "Ejercicio 2.b.1 : " esNombreLargo persona1
  mostrarTextoYresultado "Ejercicio 2.b.2 : " esNombreLargo persona2

  ---------------------------------------------------------
  -- 2 b
  ---------------------------------------------------------

  mostrarTextoYresultado "la persona3 " esSuertuda persona3
  mostrarTextoYresultado "la persona4 " esSuertuda persona4

  ---------------------------------------------------------
  -- 2 c
  ---------------------------------------------------------

  mostrarTextoYresultado "la persona 3 " tieneNombreLindo persona3
  mostrarTextoYresultado "la persona 4 " tieneNombreLindo persona4

  ---------------------------------------------------------
  -- 3
  ---------------------------------------------------------

  print (recibirseCarrera "arquitectura" persona5)
  print (viajarListaCiudades ["Roma", "Barcelona", "Valencia"] persona5)
  print (enamorarseOtraPersona persona5 persona2) 
  print (queTodoSigaIgual persona5)
  print (comboPerfecto persona5)
