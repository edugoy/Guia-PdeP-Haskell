--Este ejercicio alguna vez se planteó como un Desafío Café con Leche: 
--Implementar la función esCuadradoPerfecto/1, sin hacer operaciones con punto flotante. 
--Ayuda: les va a venir bien una función auxiliar, tal vez de dos parámetros. Pensar que el primer cuadrado perfecto es 0, 
--para llegar al 2do (1) sumo 1, para llegar al 3ro (4) sumo 3, para llegar al siguiente (9) sumo 5, después sumo 7, 9, 11 etc.. 
--También algo de recursividad van a tener que usar. 

esCuadradoPerfecto :: Int -> Bool
esCuadradoPerfecto n = esCuadradoDesde n 1 0

esCuadradoDesde :: Int -> Int -> Int -> Bool
esCuadradoDesde objetivo impar suma 
    | suma == objetivo = True
    | suma > objetivo = False
    | otherwise     = esCuadradoDesde objetivo (impar + 2) (suma + impar)