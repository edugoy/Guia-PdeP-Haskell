--a)
crecimientoAnual :: Int -> Int
crecimientoAnual edad 
    | edad < 10 = 24 - (edad *2)
    | edad <= 15 = 4
    | edad <= 17 = 2
    | edad <= 19 = 1
    | otherwise = 0

--b)
crecimientoEntreEdades :: Int -> Int -> Int
crecimientoEntreEdades edadInicial edadFinal = sum (map crecimientoAnual [edadInicial .. edadFinal - 1])

--c)
alturasEnUnAnio :: Int -> [Int] -> [Int]
alturasEnUnAnio edad lista = map (+ crecimientoAnual edad) lista

--d)
alturaEnEdades :: Int -> Int -> [Int] -> [Int]
alturaEnEdades altura edad lista = map ((altura +) . (crecimientoEntreEdades edad)) lista