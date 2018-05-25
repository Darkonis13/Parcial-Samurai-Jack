module SamuraiJack where
import Text.Show.Functions

data Elemento = UnElemento { tipo :: String, ataque :: (Personaje-> Personaje), defensa :: (Personaje-> Personaje) } deriving (Show)
data Personaje = UnPersonaje { nombre :: String, salud :: Float, elementos :: [Elemento], anioPresente :: Int}

--EJEMPLOS DE PERSONAJE Y ELEMENTO
aku = UnPersonaje { nombre = "Aku", salud = 200, elementos =  [maldadPura], anioPresente = 2058}
maldadPura = UnElemento {tipo = "Maldad", ataque = meditar, defensa = meditar}
--PUNTO 1

mandarAlAnio :: Personaje->Int->Personaje
mandarAlAnio unPersonaje anio = unPersonaje {anioPresente = anio}

meditar :: Personaje->Personaje
meditar unPersonaje = unPersonaje {salud = salud unPersonaje + (salud unPersonaje/ 2)}

causarDanio :: Personaje -> Float-> Personaje
causarDanio unPersonaje danio
 |salud unPersonaje - danio < 0 = unPersonaje{salud = 0}
 |otherwise = unPersonaje {salud = salud unPersonaje - danio}

--PUNTO 2

--A

esMalvado :: Personaje ->Bool
esMalvado unPersonaje = verificarMaldad (elementos unPersonaje)

verificarMaldad :: [Elemento] -> Bool
verificarMaldad = any ((=="Maldad").(tipo))

--B

danioQueProduce :: Personaje -> Elemento -> Float
danioQueProduce unPersonaje unElemento = salud unPersonaje - (salud.(ataque unElemento)) unPersonaje

--C

enemigosMortales :: Personaje -> [Personaje] -> [Personaje]
enemigosMortales unPersonaje enemigos = filter ((==True).esMortal unPersonaje) enemigos

esMortal :: Personaje -> Personaje -> Bool
esMortal unPersonaje unEnemigo = any ((==0).(elementoMortal unPersonaje)) (elementos unEnemigo)

elementoMortal :: Personaje -> Elemento -> Float
elementoMortal unPersonaje unElemento = (danioQueProduce unPersonaje) unElemento 

--PUNTO 3

--A

concentracion = UnElemento {tipo = "Magia",ataque = id, defensa = concentrarse}
esbirrosMalvados = UnElemento {tipo = "Maldad",ataque = invocarEsbirro, defensa = id}
esbirro = UnElemento {tipo = "Maldad", ataque = causa1Danio, defensa = id}

concentrarse :: Personaje -> Personaje
concentrarse = meditar.meditar.meditar

causa1Danio :: Personaje -> Personaje
causa1Danio unPersonaje = causarDanio unPersonaje 1

invocarEsbirro :: Personaje -> Personaje
invocarEsbirro unPersonaje =  unPersonaje {elementos = elementos unPersonaje ++ (replicate 3 esbirro)}
