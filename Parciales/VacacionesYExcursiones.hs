data Turista = Turista {
    cansancio :: Int,
    estres :: Int,
    solitario :: Bool,
    idiomaQueHabla :: [Idioma]
}
type Idioma = String

--1) modelar turistas
ana :: Turista
ana = Turista {cansancio = 0, estres = 21, solitario = False, idiomaQueHabla = ["espaniol"]}

beto :: Turista
beto = Turista {cansancio = 15 , estres = 15 , solitario = True, idiomaQueHabla = ["Aleman"]}

cathi :: Turista
cathi = Turista {cansancio = 15 , estres = 15 , solitario = True, idiomaQueHabla = ["Aleman","Catalan"]}

--funciones para cambiar datos
cambiarCansanacio :: Int -> Turista -> Turista
cambiarCansanacio numero unTurista = unTurista {cansancio = cansancio unTurista + numero}

cambiarEstres :: Int -> Turista -> Turista
cambiarEstres numero unTurista = unTurista {estres = estres unTurista + numero}

vaConAlguien :: Turista -> Turista
vaConAlguien unTurista = unTurista {solitario = False}

aprenderIdioma :: Idioma -> Turista -> Turista
aprenderIdioma unIdioma unTurista = unTurista {idiomaQueHabla = unIdioma : idiomaQueHabla unTurista} 

--2) modelado de excursiones
type Excursion = Turista -> Turista

irALaPlaya :: Excursion
irALaPlaya unTurista 
    | solitario unTurista = cambiarCansanacio (-5) unTurista
    | otherwise = cambiarEstres (-1) unTurista 

apreciarElementoDelPaisaje :: String -> Excursion
apreciarElementoDelPaisaje elemento unTurista = cambiarEstres (- elementoVisto elemento) unTurista

elementoVisto :: String -> Int
elementoVisto = length

salirAHablarIdioma :: Idioma -> Excursion 
salirAHablarIdioma unIdoma unTurista = aprenderIdioma unIdoma . vaConAlguien $ unTurista

caminar :: Int -> Excursion
caminar minutos unTurista = cambiarCansanacio (intensidad minutos) . cambiarEstres (- intensidad minutos) $ unTurista

intensidad :: Int -> Int
intensidad minutos = div minutos 4

paseoEnBarco :: Int -> Excursion
paseoEnBarco marea unTurista 
    | estaFuerte marea = cambiarEstres 6 . cambiarCansanacio 10 $ unTurista
    | estaTranquila marea = caminar 10 . apreciarElementoDelPaisaje "mar" . salirAHablarIdioma "Aleman" $ unTurista
    | otherwise = unTurista

estaFuerte :: Int -> Bool
estaFuerte marea = marea > 20

estaTranquila :: Int -> Bool
estaTranquila marea = marea > 10

--a)
hacerUnaExcursion :: Turista -> Excursion -> Turista
hacerUnaExcursion unTurista unaExcursion = unaExcursion . cambiarEstres (- porcentajeEstres unTurista 10) $ unTurista

porcentajeEstres :: Turista -> Int -> Int
porcentajeEstres unTurista porcentaje = estres unTurista `div` porcentaje

--b)
--funcion dada en el examen
deltaSegun :: (a -> Int) -> a -> a -> Int
deltaSegun f algo1 algo2 = f algo1 - f algo2

type Indice = (Turista -> Int)

deltaExcursionSegun :: Indice -> Turista -> Excursion -> Int
deltaExcursionSegun unIndice unTurista unaExcursion =
    deltaSegun unIndice (hacerUnaExcursion unTurista unaExcursion) unTurista

--c)
excursionEducativa :: Turista -> Excursion -> Bool
excursionEducativa unTurista unaExcursion = (> 0) . deltaExcursionSegun (length . idiomaQueHabla) unTurista $ unaExcursion

excursionDesestresante :: Turista -> [Excursion] -> [Excursion]
excursionDesestresante unTurista listaExcursiones = filter (esDesestresante unTurista) listaExcursiones

esDesestresante :: Turista -> Excursion -> Bool
esDesestresante unTurista unaExcursion = (<= -3) .deltaExcursionSegun estres unTurista $ unaExcursion

