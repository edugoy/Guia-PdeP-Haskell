-- En una plantación de pinos, de cada árbol se conoce la altura expresada en cm. 
--El peso de un pino se puede calcular a partir de la altura así: 3 kg x cm hasta 3 metros, 2 kg x cm arriba de los 3 metros. 
--P.ej. 2 metros ⇒  600 kg, 5 metros ⇒  1300 kg. 
--Los pinos se usan para llevarlos a una fábrica de muebles, a la que le sirven árboles de entre 400 y 1000 kilos, 
--un pino fuera de este rango no le sirve a la fábrica. Para esta situación:

--a) Definir la función pesoPino, recibe la altura de un pino y devuelve su peso. 
--b) Definir la función esPesoUtil, recibe un peso en kg y devuelve True si un pino de ese peso le sirve a la fábrica, 
--y False en caso contrario. 
--c) Definir la función sirvePino, recibe la altura de un pino y devuelve True si un pino de ese peso le sirve a la fábrica, 
--y False en caso contrario. Usar composición en la definición. 

--a)
pesoPino :: Int -> Int
pesoPino altura 
    | altura <= 300 = altura * 3
    | otherwise = ((altura - sobrante altura) * 3)+ (sobrante altura * 2)

sobrante :: Int -> Int
sobrante altura = altura - 300

--b)
esPesoUtil :: Int -> Bool
esPesoUtil peso = peso >= 400 && peso <= 1000

--c)
sirvePino :: Int -> Bool
--sirvePino altura = (esPesoUtil (pesoPino altura)) "Funcion orginal, antes de usar composicion"
sirvePino = (esPesoUtil . pesoPino) --Funcion despues de usar la composicion