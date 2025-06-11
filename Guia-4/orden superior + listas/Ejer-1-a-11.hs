--1) Definir la función esMultiploDeAlguno/2, que recibe un número y una lista y devuelve True si el número es múltiplo de alguno de 
--los números de la lista. 
esMultiploDeAlguno :: Int -> [Int] -> Bool
esMultiploDeAlguno x = any (multiplo x)

multiplo :: Int -> Int -> Bool
multiplo x y = mod x y == 0

--2) Armar una función promedios/1, que dada una lista de listas me devuelve la lista de los promedios de cada lista-elemento.
promedios :: [[Int]] -> [Float]
promedios = map promedioLista

promedioLista :: [Int] -> Float
promedioLista lista = fromIntegral (sum lista) / fromIntegral (length lista)

--3) Armar una función promediosSinAplazos que dada una lista de listas me devuelve la lista de los promedios de cada lista-elemento, 
--excluyendo los que sean menores a 4 que no se cuentan.
promediosSinAplazos :: [[Int]] -> [Float]
promediosSinAplazos = map (promedioLista . filter(>3))

--4) Definir la función mejoresNotas, que dada la información de un curso devuelve la lista con la mejor nota de cada alumno.
mejoresNotas :: [[Int]] -> [Int]
mejoresNotas = map maximum

--5) Definir la función aprobó/1, que dada la lista de las notas de un alumno devuelve True si el alumno aprobó. 
--Se dice que un alumno aprobó si todas sus notas son 6 o más.
aprobo :: [Int] -> Bool
aprobo lista = not (any (<6) lista)

--6) Definir la función aprobaron/1, que dada la información de un curso devuelve la información de los alumnos que aprobaron
aprobaron :: [[Int]] -> [[Int]]
aprobaron = filter aprobo

--7) Definir la función divisores/1, que recibe un número y devuelve la lista de divisores.
divisores :: Int -> [Int]
divisores n = filter (esDivisorDe n) [1..n]

esDivisorDe :: Int -> Int -> Bool
esDivisorDe n x = mod n x == 0

--8) Definir la función exists/2, que dadas una función booleana y una lista devuelve True si la función da True para algún 
--elemento de la lista.
exists :: (a -> Bool) -> [a] -> Bool
exists = any

--9) Definir la función hayAlgunNegativo/2, que dada una lista de números y un (…algo…) devuelve True si hay algún nro. negativo.
hayAlgunNegativo :: [Int] -> (Int -> Bool) -> Bool
hayAlgunNegativo lista fun = any fun lista

--10) Definir la función aplicarFunciones/2, que dadas una lista de funciones y un valor cualquiera, 
--devuelve la lista del resultado de aplicar las funciones al valor
aplicarFunciones :: [a -> b] -> a -> [b]
aplicarFunciones funciones n = map ($ n) funciones

--11) Definir sumaF/2, que dadas una lista de funciones y un número, devuelve la suma del resultado de aplicar las funciones al número
sumaF :: Num b => [a -> b] -> a -> b
sumaF funciones n = sum (map ($ n) funciones)