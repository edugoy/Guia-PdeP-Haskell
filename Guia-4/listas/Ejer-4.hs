-- Se tiene información detallada de la duración en minutos de las llamadas que se llevaron a cabo en un período determinado, 
--discriminadas en horario normal y horario reducido. 
--duracionLlamadas = 
--(("horarioReducido",[20,10,25,15]),(“horarioNormal”,[10,5,8,2,9,10])).

--a) Definir la función cuandoHabloMasMinutos, devuelve en que horario se habló más cantidad de minutos, en el de tarifa normal o 
--en el reducido. 
--Main> cuandoHabloMasMinutos 
--"horarioReducido"

--b)Definir la función cuandoHizoMasLlamadas, devuelve en que franja horaria realizó más cantidad de llamadas, en el de tarifa 
--normal o en el reducido. 
--Main> cuandoHizoMasLlamadas 
--"horarioNormal” 
--Nota: Utilizar composición en ambos casos 

--a)
duracionLlamadas :: ((String,[Int]),(String,[Int]))
duracionLlamadas = (("horarioReducido" ,[20,10,25,15]),("horarioNormal" ,[10,5,8,2,9,10]))

cuandoHabloMasMinutos :: String
cuandoHabloMasMinutos
    | sum (snd (fst duracionLlamadas)) > sum (snd (snd duracionLlamadas)) = fst (fst duracionLlamadas)
    | otherwise = fst (snd duracionLlamadas)

--b)
cuandoHizoMasLlamadas :: String
cuandoHizoMasLlamadas 
    | length (snd (fst duracionLlamadas)) > length (snd (snd duracionLlamadas)) = fst (fst duracionLlamadas)
    | otherwise = fst (snd duracionLlamadas)