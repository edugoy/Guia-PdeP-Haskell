data Atraccion = Atraccion {
    nombre :: String,
    alturaMinima :: Float,
    duracion :: Int,
    opiniones :: [String],
    enMantenimiento :: Bool,
    puntaje :: Int,
    ordenesDeReparacion :: [(Int, String)]
}deriving(Show,Eq)

esBuenaAtraccion :: Atraccion -> Atraccion
esBuenaAtraccion a 
    | duracion a > 10 = a {puntaje = 100}
    | length (ordenesDeReparacion a) < 3 = a {puntaje = 10 * length (nombre a) + 2 * length (opiniones a)}
    | otherwise = a {alturaMinima = alturaMinima a * 10}

ajusteDeTornilleria :: Atraccion -> Int -> Atraccion
ajusteDeTornilleria a tornillos = actualizarMantenimiento(a {duracion = min 10 (duracion a + tornillos)})

engrase :: Atraccion -> Float -> Atraccion
engrase a grasa = actualizarMantenimiento (a {alturaMinima = alturaMinima a + (grasa * 0.1), opiniones = "para valientes" : opiniones a})

mantenimientoElectrico :: Atraccion -> Atraccion
mantenimientoElectrico a = actualizarMantenimiento (a {opiniones = take 2 (opiniones a)})

mantenimientoBasico :: Atraccion -> Atraccion
mantenimientoBasico a = actualizarMantenimiento(engrase (ajusteDeTornilleria a 8) 10)

actualizarMantenimiento :: Atraccion -> Atraccion
actualizarMantenimiento a 
    | null (ordenesDeReparacion a) = a {enMantenimiento = False}
    | otherwise = a {enMantenimiento = True, ordenesDeReparacion = init (ordenesDeReparacion a)}

esaMeDaMiedo :: Atraccion -> Bool
esaMeDaMiedo a = any ((> 4) . fst) (ordenesDeReparacion a)

acaCerramos :: Atraccion -> Bool
acaCerramos a = sum (map fst (ordenesDeReparacion a)) == 7

disneyNoEsistis :: [Atraccion] -> Bool
disneyNoEsistis = all (null . ordenesDeReparacion) . filter ((>5) . length . nombre)