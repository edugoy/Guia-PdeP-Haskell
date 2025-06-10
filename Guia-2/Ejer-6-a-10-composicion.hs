--6) Resolver la función del ejercicio 2 de la guía anterior esMultiploDe/2, utilizando aplicación parcial y composición.
--Ejercicio de la guia anterior:
--esMultiploDe :: Int -> Int -> Bool
--esMultiploDe x y = x `mod` y == 0

esMultiploDe :: Int -> Int -> Bool
esMultiploDe x = (== 0) . (`mod` x )

--7) Resolver la función del ejercicio 5 de la guía anterior esBisiesto/1, utilizando aplicación parcial y composición.
--Ejercicio de la guia anterior:
--esBisiesto :: Int -> Bool
--esBisiesto x = x `esMultiploDe` 400 || x `esMultiploDe` 4 && not (x `esMultiploDe` 100)

esBisiesto :: Int -> Bool
esBisiesto = (||) <$> esMultiploDe 400 <*> ((&&) <$> esMultiploDe 4 <*> (not . esMultiploDe 100))

--8) Resolver la función inversaRaizCuadrada/1, que da un número n devolver la inversa su raíz cuadrada. 
--Main> inversaRaizCuadrada 4 
--0.5 
--Nota: Resolverlo utilizando la función inversa Ej. 2.3, sqrt y composición.

--Funcion inversa:
inversa :: Float -> Float
inversa = (1/)

inversaRaizCuadrada :: Float -> Float
inversaRaizCuadrada = inversa . sqrt

--9) Definir una función incrementMCuadradoN, que invocándola con 2 números m y n, incrementa un valor m al cuadrado de n por Ej: 
--Main> incrementMCuadradoN 3 2 
--11 
--Incrementa 2 al cuadrado de 3, da como resultado 11. Nota: Resolverlo utilizando aplicación parcial y composición. 

incrementMCuadradoN :: Int -> Int -> Int
incrementMCuadradoN m = (+) (cuadrado m)

--Usamos una funcion que haga el cuadrado
cuadrado :: Int -> Int
cuadrado n = n * n 

--10) Definir una función esResultadoPar/2, que invocándola con número n y otro m, devuelve true si el resultado de elevar n a m es par. 
--Main> esResultadoPar 2 5 
--True 
--Main> esResultadoPar 3 2
--False 
--Nota Obvia: Resolverlo utilizando aplicación parcial y composición.
esResultadoPar :: Int -> Int -> Bool
esResultadoPar =(even .) . (^)