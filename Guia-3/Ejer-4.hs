--Representamos las notas que se sacó un alumno en dos parciales mediante un par (nota1,nota2), 
--p.ej. un patito en el 1ro y un 7 en el 2do se representan mediante el par (2,7). 
--A partir de esto:

--a) Definir la función esNotaBochazo, recibe un número y devuelve True si no llega a 6, False en caso contrario. No vale usar guardas

--b) Definir la función aprobo, recibe un par e indica si una persona que se sacó esas notas aprueba. Usar esNotaBochazo.

--c) Definir la función promociono, que indica si promocionó, para eso tiene las dos notas tienen que sumar al menos 15 y 
--además haberse sacado al menos 7 en cada parcial.

--d) Escribir una consulta que dado un par indica si aprobó el primer parcial, usando esNotaBochazo y composición. 
--La consulta tiene que tener esta forma (p.ej. para el par de notas (5,8)) 
--Main> (... algo ...) (5,8) 

--a)
esNotaBochazo :: Int -> Bool
esNotaBochazo = (<6)

--b)
aprobo :: (Int, Int) -> Bool
aprobo (nota1, nota2) = not (esNotaBochazo nota1) && not (esNotaBochazo nota2)

--c)
promociono :: (Int, Int) -> Bool
promociono (nota1, nota2) = (nota1 + nota2) >= 15 && nota1 > 7 || nota2 > 7

--d) esto no es una funcion sino algo que debe de ingresarse en la consola
--se debe de ingresar la siguiente composicion "(not. esNotBochazo . fst) (x,y)" donde X e Y serian las notas del alumno