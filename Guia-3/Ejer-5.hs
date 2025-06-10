--Siguiendo con el dominio del ejercicio anterior, tenemos ahora dos parciales con dos recuperatorios, 
--lo representamos mediante un par de pares ((parc1,parc2),(recup1,recup2)). 
--Si una persona no rindió un recuperatorio, entonces ponemos un "-1" en el lugar correspondiente. 
--Observamos que con la codificación elegida, siempre la mejor nota es el máximo entre nota del parcial y nota del recuperatorio. 
--Considerar que vale recuperar para promocionar. En este ejercicio vale usar las funciones que se definieron para el anterior.

--a)Definir la función notasFinales que recibe un par de pares y devuelve el par que corresponde a las notas finales del alumno para 
--el 1er y el 2do parcial.

--b)Escribir la consulta que indica si un alumno cuyas notas son ((2,7),(6,-1)) recursa o no. O sea, la respuesta debe ser True si recursa, 
--y False si no recursa. Usar las funciones definidas en este punto y el anterior, y composición

--c) Escribir la consulta que indica si un alumno cuyas notas son ((2,7),(6,-1)) recuperó el primer parcial. Usar composición

--d) Definir la función recuperoDeGusto que dado el par de pares que representa a un alumno, devuelve True si el alumno, 
--pudiendo promocionar con los parciales (o sea sin recup.), igual rindió al menos un recup. Vale definir funciones auxiliares. 
--Hacer una definición que no use pattern matching, en las eventuales funciones auxiliares tampoco; o sea, manejarse siempre con fst y snd.

--Funciones del ejercicio pasado
esNotaBochazo :: Int -> Bool
esNotaBochazo = (<6)
promociono :: (Int, Int) -> Bool
promociono (nota1, nota2) = (nota1 + nota2) >= 15 && nota1 >= 7 && nota2 >= 7

--a)
notasFinales :: ((Int, Int), (Int, Int)) -> (Int, Int)
notasFinales (parcial, recu) = ((max (fst parcial) (fst recu)),(max (snd parcial) (snd recu)))

--b) y c) son consulta a la terminal, no es una funcion de codigo

--d)
recuperoDeGusto :: ((Int, Int), (Int, Int)) -> Bool
recuperoDeGusto (parcial, recu) = promociono parcial && not (debeRecuperar recu)

debeRecuperar :: (Int, Int) -> Bool
debeRecuperar (recu1, recu2) = recu1 /= -1 && recu2 /= -1