--1)  Definir una función que sume una lista de números. 
--Nota: Investigar sum 

sumaLista :: [Int] -> Int
sumaLista = foldl (+) 0 
