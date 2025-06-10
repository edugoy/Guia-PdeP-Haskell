--Definir la función esMayorDeEdad, que dada una tupla de 2 elementos (persona, edad) me devuelva True si es mayor de 21 años y 
--False en caso contrario. Por Ej:.
--Main> esMayorDeEdad (juan,18) 
--False 
--Nota: Definir la función utilizando aplicación parcial y composición.
esMayorDeEdad :: (String, Int) -> Bool
esMayorDeEdad persona = (>= 18) (snd persona)

-- esMayorDeEdad persona = (>=) (snd persona) 18