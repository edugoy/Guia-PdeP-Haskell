data Guerrero = Guerrero {
    ki :: Float,
    fatiga :: Float,
    cansancio :: Float,
    estado :: String,
    tipo :: String
} deriving(Show,Eq)

flexionesDeBrazo :: Guerrero -> Guerrero
flexionesDeBrazo g = g {fatiga = fatiga g + 50}

saltosAlCajon :: Guerrero -> Float -> Guerrero
saltosAlCajon g altura = g {
    ki = ki g + altura / 10,
    cansancio = cansancio g + altura / 5
    }

snatch :: Guerrero -> Guerrero
snatch g 
    | seHizoBien g = g {ki = ki g * 1.05,
        fatiga = fatiga g * 1.1}
    | otherwise = g {fatiga = fatiga g +100}

seHizoBien :: Guerrero -> Bool
seHizoBien g = ki g == 22000

estadoDelGuerrero :: Guerrero -> String
estadoDelGuerrero g 
    | fatiga g > ki g * 0.72 = "exhausto"
    | fatiga g > ki g * 0.44 = "cansado"
    | otherwise = "fresco"

realizarDescanso :: Guerrero -> Int -> Guerrero
realizarDescanso g min = g { cansancio = cansancio g - fromIntegral(sum [1 .. min])}

realizarEjercicio :: (Guerrero -> Guerrero) -> Guerrero -> Guerrero
realizarEjercicio ejercicio g 
    | estadoDelGuerrero g == "exhausto" = g {ki = ki g * 0.98}
    | estadoDelGuerrero g == "cansado" = g {ki = ki g + (ki (ejercicio g) - ki g) * 2, 
        cansancio = cansancio g + (cansancio (ejercicio g) - cansancio g) * 4}
    | otherwise = ejercicio g

cantidadOptimaDescanso :: Guerrero -> Int 
cantidadOptimaDescanso g = buscarMinuto g 1

buscarMinuto :: Guerrero -> Int -> Int
buscarMinuto g n
    | cansancio g == 0 = 0
    | fromIntegral(sum [1 .. n]) >= cansancio g = n
    | otherwise = buscarMinuto g (n + 1)

realizarRutina :: Guerrero -> [Guerrero -> Guerrero] -> Guerrero
realizarRutina g ejercicios
    | tipo g == "sacado" = aplicarEjercicios ejercicios g
    | tipo g == "perezoso" = aplicarEjerciciosConDescanso ejercicios g
    | otherwise = g

aplicarEjercicios :: [Guerrero -> Guerrero] -> Guerrero -> Guerrero
aplicarEjercicios [] g = g
aplicarEjercicios (e:es) g = aplicarEjercicios es (realizarEjercicio e g)

aplicarEjerciciosConDescanso :: [Guerrero -> Guerrero] -> Guerrero -> Guerrero
aplicarEjerciciosConDescanso [] g = g
aplicarEjerciciosConDescanso (e:es) g = aplicarEjerciciosConDescanso es (realizarDescanso (realizarEjercicio e g) 5)