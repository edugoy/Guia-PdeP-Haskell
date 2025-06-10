--1) Definir las funciones fst3, snd3, trd3, que dada una tupla de 3 elementos devuelva el elemento correspondiente
fst3 ::  (a, b, c) -> a
fst3 (x, _, _) = x

snd3 :: (a, b, c) -> b
snd3 (_, y, _) = y

trd3 :: (a, b, c) -> c
trd3 (_, _, z) = z

--2)Definir la función aplicar, que recibe como argumento una tupla de 2 elementos con funciones y un entero, 
--me devuelve como resultado una tupla con el resultado de aplicar el elemento a cada una de la funciones
aplicar :: (Int -> a, Int -> b) -> Int -> (a, b)
aplicar (doble , triple ) x = (doble x , triple x)

doble :: Int -> Int
doble = (*2)

triple :: Int -> Int
triple = (*3)

--3) Definir la función cuentaBizarra, que recibe un par y: si el primer elemento es mayor al segundo devuelve la suma, 
--si el segundo le lleva más de 10 al primero devuelve la resta 2do – 1ro, y si el segundo es más grande que el 1ro pero no llega a 
--llevarle 10, devuelve el producto
cuentaBizarra :: (Int, Int) -> Int
cuentaBizarra (x, y)
    | x > y = x + y
    | (y - x) > 10 = y - x
    | y > x && (y - x) < 10 = x * y
