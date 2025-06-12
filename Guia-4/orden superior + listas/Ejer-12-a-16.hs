--12) Un programador Haskell está haciendo las cuentas para un juego de fútbol virtual (como el Hattrick o el ManagerZone). 
--En un momento le llega la información sobre la habilidad de cada jugador de un equipo, que es un número entre 0 y 12, 
--y la orden de subir la forma de todos los jugadores en un número entero; p.ej., subirle 2 la forma a cada jugador. 
--Ahora, ningún jugador puede tener más de 12 de habilidad; si un jugador tiene 11 y la orden es subir 2, pasa a 12, no a 13; 
--si estaba en 12 se queda en 12. Escribir una función subirHabilidad/2 que reciba un número (que se supone positivo sin validar) 
--y una lista de números, y le suba la habilidad a cada jugador cuidando que ninguno se pase de 12.

subirHabilidad :: Int -> [Int] -> [Int]
subirHabilidad incremento lista = map (limitarA12 incremento) lista

limitarA12 :: Int -> Int -> Int
limitarA12 inc habilidad = min 12 (inc + habilidad)

-- 13) Ahora el requerimiento es más genérico: hay que cambiar la habilidad de cada jugador según una función que recibe 
-- la vieja habilidad y devuelve la nueva. Armar: una función flimitada que recibe una función f y un número n, y devuelve 
-- f n garantizando que quede entre 0 y 12 (si f n < 0 debe devolver 0, si f n > 12 debe devolver 12)

flimitada :: (Int -> Int) -> Int -> Int
flimitada funcion num 
    | funcion num > 12 =  12
    | funcion num < 0 = 0 
    | otherwise = funcion num

--a) Definir una función cambiarHabilidad/2, que reciba una función f y una lista de habilidades, 
--y devuelva el resultado de aplicar f con las garantías de rango que da flimitada

cambiarHabilidad :: (Int -> Int) -> [Int] -> [Int]
cambiarHabilidad func habilidades = map (flimitada func) habilidades

--15) Usar takeWhile/2 para definir las siguientes funciones: primerosPares/1, que recibe una lista de números 
--y devuelve la sublista hasta el primer no par exclusive.

primerosPares :: [Int] -> [Int]
primerosPares lista = takeWhile (even) lista

--a) primerosDivisores/2, que recibe una lista de números y un número n, y devuelve la sublista hasta el primer número 
--que no es divisor de n exclusive. 

primerosDivisores :: [Int] -> Int -> [Int]
primerosDivisores lista divisor = takeWhile (esDivisor divisor) lista 

esDivisor :: Int -> Int -> Bool
esDivisor n x = mod n x == 0 

--b) primerosNoDivisores/2, que recibe una lista de números y un número n, y devuelve la sublista hasta el primer número 
--que sí es divisor de n exclusive. 

primerosNoDivisores :: [Int] -> Int -> [Int]
primerosNoDivisores lista divisor = takeWhile(noEsDivisor divisor) lista

noEsDivisor :: Int -> Int -> Bool
noEsDivisor n x = mod n x /= 0 

--16) Se representa la información sobre ingresos y egresos de una persona en cada mes de un año mediante dos listas, 
--de 12 elementos cada una. P.ej., si entre enero y junio gané 100, y entre julio y diciembre gané 120, mi lista de ingresos es:
--[100,100,100,100,100,100,120,120,120,120,120,120] 
--Si empecé en 100 y fui aumentando de a 20 por mes, llegando a 220, queda:
--[100,120..220] 
--Y si es al revés, empecé en 220 y fui bajando de a 20 por mes hasta llegar a 100, queda:
--[220,200..100] 
--(jugar un poco con esta notación) 
--Definir la función: huboMesMejorDe/3, que dadas las listas de ingresos y egresos y un número, devuelve True si 
--el resultado de algún mes es mayor que el número.

huboMesMejorDe :: [Int] -> [Int] -> Int -> Bool
huboMesMejorDe ingreso egreso num = any (>num) (zipWith (-) ingreso egreso)