data Guerrero = Guerrero {
    ki :: Float,
    fatiga :: Float,
    cansancio :: Float,
    tipo :: String
} deriving(Show,Eq)

modificarKi :: (Float -> Float) -> Guerrero -> Guerrero
modificarKi funcion guerrero = guerrero {ki = funcion (ki guerrero)}

modificarFatiga :: (Float -> Float) -> Guerrero -> Guerrero
modificarFatiga funcion guerrero = guerrero {fatiga = funcion (fatiga guerrero)}

modificarCansancio :: (Float -> Float) -> Guerrero -> Guerrero
modificarCansancio funcion guerrero = guerrero {cansancio = funcion (cansancio guerrero)}

flexionesDeBrazo :: Guerrero -> Guerrero
flexionesDeBrazo = modificarFatiga (+ 50)

saltosAlCajon ::Float -> Guerrero ->  Guerrero
saltosAlCajon altura guerrero = modificarCansancio (+ altura / 5) (modificarKi (+ altura / 10) guerrero)

snatch :: Guerrero -> Guerrero
snatch guerrero 
    | seHizoBien guerrero = modificarKi (* 1.05) (modificarFatiga (* 1.1) guerrero)
    | otherwise = modificarFatiga (+ 100) guerrero

seHizoBien :: Guerrero -> Bool
seHizoBien guerrero = ki guerrero == 22000

realizarDescanso :: Guerrero -> Float -> Guerrero
realizarDescanso guerrero minuto = modificarCansancio (restarCansancioPorMinuto minuto) guerrero

restarCansancioPorMinuto :: Float -> Float -> Float
restarCansancioPorMinuto minutos cansancio = cansancio - sum [1 .. minutos]

realizarEjercicio :: (Guerrero -> Guerrero) -> Guerrero -> Guerrero
realizarEjercicio ejercicio guerrero 
    | estaExhausto guerrero = modificarKi (* 0.98) guerrero
    | estaCansado guerrero = guerrero {ki = ki guerrero + (ki (ejercicio guerrero) - ki guerrero) * 2, 
        cansancio = cansancio guerrero + (cansancio (ejercicio guerrero) - cansancio guerrero) * 4}
    | otherwise = ejercicio guerrero

estaExhausto :: Guerrero -> Bool
estaExhausto guerrero = fatiga guerrero > ki guerrero * 0.72

estaCansado :: Guerrero -> Bool
estaCansado guerrero = fatiga guerrero > ki guerrero * 0.44

cantidadOptimaDescanso :: Guerrero -> Int 
cantidadOptimaDescanso guerrero = buscarMinuto guerrero 1

buscarMinuto :: Guerrero -> Int -> Int
buscarMinuto guerrero n
    | cansancio guerrero == 0 = 0
    | fromIntegral(sum [1 .. n]) >= cansancio guerrero = n
    | otherwise = buscarMinuto guerrero (n + 1)

realizarRutina :: Guerrero -> [Guerrero -> Guerrero] -> Guerrero
realizarRutina guerrero ejercicios
    | tipo guerrero == "sacado" = aplicarEjercicios ejercicios guerrero
    | tipo guerrero == "perezoso" = aplicarEjerciciosConDescanso ejercicios guerrero
    | otherwise = guerrero

aplicarEjercicios :: [Guerrero -> Guerrero] -> Guerrero -> Guerrero
aplicarEjercicios [] guerrero = guerrero
aplicarEjercicios (e:es) guerrero = aplicarEjercicios es (realizarEjercicio e guerrero)

aplicarEjerciciosConDescanso :: [Guerrero -> Guerrero] -> Guerrero -> Guerrero
aplicarEjerciciosConDescanso [] guerrero = guerrero
aplicarEjerciciosConDescanso (e:es) guerrero = aplicarEjerciciosConDescanso es (realizarDescanso (realizarEjercicio e guerrero) 5)