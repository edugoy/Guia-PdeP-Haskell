data Heroe = Heroe {
    epipteo :: String,
    reconocimiento :: Int,
    artefactos :: [Artefacto],
    tareas :: [Tarea]
}

type Artefacto = (Int, String)
type Tarea = Heroe -> Heroe

pasarALaHistoria :: Heroe -> Heroe
pasarALaHistoria heroe
    | reconocimiento heroe > 1000 = modificarEpipteo "El mitico" heroe
    | reconocimiento heroe >= 500 = modificarEpipteo "El magnifico" . agregarArtefacto (100,"Lanza del Olimpo") $ heroe
    | reconocimiento heroe > 100 = modificarEpipteo "Hoplita" . agregarArtefacto (50,"Xiphos") $ heroe
    | otherwise = heroe

modificarEpipteo :: String -> Heroe ->  Heroe
modificarEpipteo nuevoNombre heroe = heroe {epipteo = nuevoNombre}

agregarArtefacto :: Artefacto -> Heroe -> Heroe
agregarArtefacto artefacto heroe = mapArtefacto (artefacto :) heroe

mapArtefacto :: ([Artefacto] -> [Artefacto]) -> Heroe -> Heroe
mapArtefacto funcion heroe = heroe {artefactos = funcion (artefactos heroe)}

encontrarArtefacto :: Artefacto -> Tarea
encontrarArtefacto artefacto = ganarReconocimiento (rareza artefacto) . agregarArtefacto artefacto

rareza :: Artefacto -> Int
rareza = fst

ganarReconocimiento :: Int -> Heroe -> Heroe
ganarReconocimiento unReconocimiento = modificarReconocimiento (+ unReconocimiento)

modificarReconocimiento :: (Int -> Int) -> Heroe -> Heroe
modificarReconocimiento funcion heroe = heroe {reconocimiento = funcion(reconocimiento heroe)}

escalarOlimpo :: Tarea
escalarOlimpo = ganarReconocimiento 500 . agregarArtefacto remlampagoDeZeus . triplicarRareza . desecharComunes

remlampagoDeZeus :: Artefacto
remlampagoDeZeus = (500, "El relampago de Zeus")

triplicarRareza :: Heroe -> Heroe
triplicarRareza heroe = mapArtefacto triplicarRarezaArtefactos heroe

triplicarRarezaArtefactos :: [Artefacto] -> [Artefacto]
triplicarRarezaArtefactos artefacto = map (mapRareza (*3)) artefacto

mapRareza :: (Int -> Int) -> Artefacto -> Artefacto
mapRareza funcion (rareza, nombre) = (funcion rareza, nombre)

desecharComunes :: Heroe -> Heroe
desecharComunes heroe = mapArtefacto (filter esComun) heroe

esComun :: Artefacto -> Bool
esComun = (> 1000) . rareza

ayudarACruzarLaCalle :: Int -> Tarea
ayudarACruzarLaCalle cuadras = modificarEpipteo ("Gros" ++ replicate cuadras 'o')

matarUnaBestia :: Bestia -> Tarea 
matarUnaBestia bestia heroe
    |(debilidad bestia) heroe = modificarEpipteo ("El asesino de " ++ nombre bestia) heroe
    | otherwise = modificarEpipteo "El cobarde" . mapArtefacto (tail) $ heroe

data Bestia = Bestia {
    nombre :: String,
    debilidad :: Heroe -> Bool
}

heracles :: Heroe
heracles = Heroe {epipteo = "Guardian del Olimpo", reconocimiento = 700, artefactos = [(1000, "Pistola"), remlampagoDeZeus], tareas = [matarAlLeonDeNemena]}

matarAlLeonDeNemena :: Tarea
matarAlLeonDeNemena = matarUnaBestia leonDeNemena

leonDeNemena :: Bestia
leonDeNemena = Bestia {nombre = "Leon de Nemena", debilidad = ((>= 20) . length . epipteo)}

hacerUnaTarea :: Tarea -> Heroe -> Heroe
hacerUnaTarea tarea heroe = agregarTarea tarea (tarea heroe)

agregarTarea :: Tarea -> Heroe -> Heroe
agregarTarea tarea heroe = heroe {tareas = tarea : tareas heroe}

presumir :: Heroe -> Heroe -> (Heroe,Heroe)
presumir heroe1 heroe2
    | leganaA heroe1 heroe2 = (heroe1, heroe2)
    | leganaA heroe2 heroe1 = (heroe2, heroe1)
    | otherwise = presumir (hacerTareaDe heroe1 heroe2)(hacerTareaDe heroe2 heroe1)

leganaA :: Heroe -> Heroe -> Bool
leganaA ganador perdedor = reconocimiento ganador > reconocimiento perdedor ||
    reconocimiento ganador == reconocimiento perdedor && sumarRareza ganador > sumarRareza perdedor

sumarRareza :: Heroe -> Int
sumarRareza = sum . map rareza . artefactos

hacerTareaDe :: Heroe -> Heroe -> Heroe
hacerTareaDe unHeroe otroHeroe = realizarTareas unHeroe (tareas otroHeroe)

realizarTareas :: Heroe -> [Tarea] -> Heroe
realizarTareas unHeroe tareas = foldr hacerUnaTarea unHeroe tareas

type Labor = [Tarea]
labor :: Labor -> Heroe -> Heroe
labor unLabor unHeroe = realizarTareas unHeroe unLabor