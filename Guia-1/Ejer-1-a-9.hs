import System.Win32.Types (FLOAT)
--1) Definir la función esMultiploDeTres/1, que devuelve True si un número es múltiplo de 3
esMultiploDeTres :: Int -> Bool
esMultiploDeTres x = x `mod` 3 == 0

--2) Definir la función esMultiploDe/2, que devuelve True si el segundo es múltiplo del primero
esMultiploDe :: Int -> Int -> Bool
esMultiploDe x y = x `mod` y == 0

--3) Definir la función cubo/1, devuelve el cubo de un número
cuboDe :: Int -> Int
cuboDe x = x * x

--4) Definir la función area/2, devuelve el área de un rectángulo a partir de su base y su altura.
area :: Int -> Int -> Int
area x y = x * y 

--5) Definir la función esBisiesto/1, indica si un año es bisiesto. 
--(Un año es bisiesto si es divisible por 400 o es divisible por 4 pero no es divisible por 100) 
--Nota: Resolverlo reutilizando la función esMultiploDe/2
esBisiesto :: Int -> Bool
esBisiesto x = x `esMultiploDe` 400 || x `esMultiploDe` 4 && not (x `esMultiploDe` 100)

--6) Definir la función celsiusToFahr/1, pasa una temperatura en grados Celsius a grados Fahrenheit
celsiusToFahr :: Float -> Float
celsiusToFahr x = (x * 9/5) + 32

--7) Definir la función fahrToCelsius/1, la inversa de la anterior.
fahrToCelsius :: Float -> Float
fahrToCelsius x = (x - 32) * 5/9

--8) Definir la función haceFrioF/1, indica si una temperatura expresada en grados Fahrenheit es fría. 
--Decimos que hace frío si la temperatura es menor a 8 grados Celsius.
haceFrioF :: Float -> Bool
haceFrioF x = (fahrToCelsius x) < 8

--9) Definir la función mcm/2 que devuelva el mínimo común múltiplo entre dos números, de acuerdo a esta fórmula. 
-- m.c.m.(a, b) = {a * b} / {m.c.d.(a, b)
-- Nota: Se puede utilizar gcd. (gcs es mcd)
mcm :: Int -> Int -> Int
mcm x y = (x * y) `div` (gcd x y)