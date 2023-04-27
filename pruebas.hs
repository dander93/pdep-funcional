{-# LANGUAGE BlockArguments #-}

import Solucion

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



main :: IO ()
main = do

---------------------------------------------------------
-- 1 a
---------------------------------------------------------

  let res1a1 = obtenerCoeficienteSatisfaccion persona1
  let result1a1 = "Ejercicio 1.a.1 " ++ show res1a1
  print result1a1

  let res1a2 = obtenerCoeficienteSatisfaccion persona2
  let result1a2 = "Ejercicio 1.a.2 " ++ show res1a2
  print result1a2

  let res1a3 = obtenerCoeficienteSatisfaccion persona3
  let result1a3 = "Ejercicio 1.a.3 " ++ show res1a3
  print result1a3

---------------------------------------------------------
-- 1 b
---------------------------------------------------------

  let res1b1 = obtenerAmbicion persona1
  let result1b1 = "Ejercicio 1.b.1 " ++ show res1b1
  print result1b1
  
  let res1b2 = obtenerAmbicion persona2
  let result1b1 = "Ejercicio 1.b.2 " ++ show res1b2
  print result1b1

  let res1b3 = obtenerAmbicion persona3
  let result1b3 = "Ejercicio 1.b.3 " ++ show res1b3
  print result1b3

---------------------------------------------------------
-- 2 a
---------------------------------------------------------

  let result2a1 = esNombreLargo persona1
  let result2a1txt = "Ejercicio 2.b.1 : " ++ obtenerNombre persona1 ++ " " ++ result2a1
  print result2a1txt

  let result2a2 = esNombreLargo persona2
  let result2a2text = "Ejercicio 2.b.2 : " ++ obtenerNombre persona2 ++ " " ++ result2a2
  print result2a2text

---------------------------------------------------------
-- 2 b
---------------------------------------------------------

  let result2b1 = esSuertuda persona3
  let result2b1text = "la persona es 3 " ++ result2b1
  print result2b1text

  let result2b2 = esSuertuda persona4
  let result2b2text = "la persona es 4 " ++ result2b2
  print result2b2text

---------------------------------------------------------
-- 2 c
---------------------------------------------------------

  let result2c1 = tieneNombreLindo persona3
  let result2c1text = "la persona 3 " ++ result2c1
  print result2c1text

  let result2c2 = tieneNombreLindo persona4
  let result2c2text = "la persona 4 " ++ result2c2
  print result2c2text

---------------------------------------------------------
-- 3
---------------------------------------------------------

  let result3a1 = recibirseCarrera "arquitectura" persona5
  let result3a1text = show result3a1
  print result3a1text

  let result3a2 = viajarListaCiudades ["Roma","Barcelona","Valencia"] persona5
  let result3a2text = show result3a2
  print result3a2text

  let result3a3 = enamorarseOtraPersona persona5 persona2
  let result3a3text = show result3a3
  print result3a3text

  let result3a4 = queTodoSigaIgual persona5
  let result3a4txt = show result3a4
  print result3a4txt

  let result3a5 = comboPerfecto persona5
  let result3a5txt = show result3a5
  print result3a5txt