import GHC.Base (BCO)
--1)Definir la función existsAny/2, que dadas una función booleana y una tupla de tres elementos devuelve True 
--si existe algún elemento de la tupla que haga verdadera la función. 

existAny :: (a ->Bool) -> (a, a, a) -> Bool
existAny funcion (x, y, z) = any funcion [x, y, z]

--2) Definir la función mejor/3, que recibe dos funciones y un número, y devuelve el resultado de la función que dé 
--un valor más alto

mejor :: Ord b => (a -> b) -> (a -> b) -> a -> b
mejor fun1 fun2 x = max (fun1 x) (fun2 x) 

cuadrado :: Int -> Int
cuadrado x = x ^ 2

triple :: Int -> Int
triple x = x * 3

--3)Definir la función aplicarPar/2, que recibe una función y un par, y devuelve el par que resulta de aplicar la función 
--a los elementos del par.

aplicarPar :: (a -> b) -> (a, a) -> (b, b)
aplicarPar funcion (x, y) =  (funcion x, funcion y)

--4) Definir la función parDeFns/3, que recibe dos funciones y un valor, y devuelve un par ordenado que es el resultado de
--aplicar las dos funciones al valor.

parDeFns :: (a -> b) -> (a -> b) -> a -> (b, b)
parDeFns fun1 fun2 x = (fun1 x, fun2 x)