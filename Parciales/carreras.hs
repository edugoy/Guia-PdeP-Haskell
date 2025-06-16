data Auto = Auto {
    color :: String,
    velocidad :: Int,
    distancia :: Int
}deriving(Show,Eq)

data Carrera = Carrera {
    estadoActual :: [Auto]
}deriving(Show,Eq)

estaCerca :: Auto -> Auto -> Bool
estaCerca auto1 auto2 = color auto1 /= color auto2 && abs (distancia auto1 - distancia auto2) < 10

vaTranquilo :: Auto -> [Auto] -> Bool
vaTranquilo auto carrera = all (not . estaCerca auto) carrera && leGanaATodos auto carrera

carreraSinMi :: Auto -> [Auto] -> [Auto]
carreraSinMi auto = filter (/= auto)

leGanaATodos :: Auto -> [Auto] -> Bool
leGanaATodos auto = all (leGanaA auto) . carreraSinMi auto

leGanaA :: Auto -> Auto -> Bool
leGanaA auto otroAuto = distancia auto > distancia otroAuto

meGanan :: Auto -> Auto -> Bool
meGanan auto otroAuto = distancia otroAuto > distancia auto

puesto :: Auto -> [Auto] -> Int
puesto auto carrera = 1 + length (filter (meGanan auto)(carreraSinMi auto carrera))

corraUnTiempo :: Auto -> Int ->Auto
corraUnTiempo auto tiempo = auto {distancia = distancia auto + tiempo * velocidad auto} 