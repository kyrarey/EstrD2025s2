-- PRACTICA 2
-- 1 RECURSION SOBRE LISTAS
-- 1
sumatoria :: [Int] -> Int
sumatoria [] = 0
sumatoria (x : xs) = x + sumatoria xs

-- 2
longitud :: [a] -> Int
longitud [] = 0
longitud (x : xs) = 1 + longitud xs

-- 3
sucesores :: [Int] -> [Int]
sucesores [] = []
sucesores (x : xs) = x + 1 : sucesores xs

-- 4
conjuncion :: [Bool] -> Bool
conjuncion [] = True
conjuncion (x : xs) = x && conjuncion xs

-- 5
disyuncion :: [Bool] -> Bool
disyuncion [] = False
disyuncion (x : xs) = x || disyuncion xs

-- 6
aplanar :: [[a]] -> [a]
aplanar [] = []
aplanar (xs : xss) = xs ++ aplanar xss

-- 7
pertenece :: (Eq a) => a -> [a] -> Bool
pertenece _ [] = False
pertenece e (x : xs) = x == e || pertenece e xs

-- 8
apariciones :: (Eq a) => a -> [a] -> Int
apariciones _ [] = 0
apariciones e (x : xs) =
  if x == e
    then 1 + apariciones e xs
    else apariciones e xs

-- 9
losMenoresA :: Int -> [Int] -> [Int]
losMenoresA _ [] = []
losMenoresA n (x : xs) =
  if n > x
    then x : losMenoresA n xs
    else losMenoresA n xs

-- 10
lasDeLongitudMayorA :: Int -> [[a]] -> [[a]]
lasDeLongitudMayorA _ [] = []
lasDeLongitudMayorA n (x : xs) =
  if longitud x > n -- x es una lista y xs es una lista de listas
    then x : lasDeLongitudMayorA n xs
    else lasDeLongitudMayorA n xs

-- 11
agregarAlFinal :: [a] -> a -> [a]
agregarAlFinal [] e = [e]
agregarAlFinal (x : xs) e = x : agregarAlFinal xs e

-- 12
agregar :: [a] -> [a] -> [a]
agregar xs [] = xs
agregar [] ys = ys
agregar (x : xs) ys = x : agregar xs ys

-- 13
reversa :: [a] -> [a]
reversa [] = []
reversa (x : xs) = reversa xs ++ [x] -- por qué no (:)?

-- 14
zipMaximos :: [Int] -> [Int] -> [Int]
zipMaximos xs [] = xs
zipMaximos [] ys = ys
zipMaximos (x : xs) (y : ys) =
  if x > y
    then x : zipMaximos xs ys
    else y : zipMaximos xs ys

-- 15
elMinimo :: (Ord a) => [a] -> a -- que es ord?
-- precon : no debe ser una lista vacia
elMinimo [] = error "No debe darse una lista vacia"
elMinimo [x] = x
elMinimo (x : xs) =
  if x < elMinimo xs
    then x
    else elMinimo xs


-- 2 RECURSION SOBRE NUMEROS
-- 1
factorial :: Int -> Int
-- precondición: n no debe ser menor a 0
factorial 0 = 1
factorial n = n * factorial (n - 1)

antecesor :: Int -> Int
antecesor n = n - 1

-- 2
cuentaRegresiva :: Int -> [Int]
cuentaRegresiva 0 = []
cuentaRegresiva n = if n<1
                      then []
                      else n : cuentaRegresiva (antecesor n)

-- 3
repetir :: Int -> a -> [a]
repetir 0 e = []
repetir n e = e : repetir (antecesor n) e

-- 4
losPrimeros :: Int -> [a] -> [a]
losPrimeros 0 xs = []
losPrimeros _ [] = []
losPrimeros n (x:xs) = x : losPrimeros (antecesor n) xs

--5
sinLosPrimeros :: Int -> [a] -> [a]
sinLosPrimeros 0 xs = xs 
sinLosPrimeros _ [] = []
sinLosPrimeros n (x:xs) = sinLosPrimeros (antecesor n) xs


-- 3 REGISTROS
-- 1.1
data Persona = P String Int -- Nombre Edad
  deriving (Show)

edad :: Persona -> Int
edad (P n e) = e

mayoresA :: Int -> [Persona] -> [Persona]
mayoresA _ [] = []
mayoresA e (p: ps) = if edad p > e 
                        then p : mayoresA e ps
                        else  mayoresA e ps

-- 1.2
promedioEdad :: [Persona] -> Int
--Precondición: la lista al menos posee una persona.
promedioEdad [] = error "La lista dada no contiene personas"
promedioEdad [p] = edad p
promedioEdad ps = promedioEn (edades ps)

edades :: [Persona] -> [Int]
edades [] = []
edades (p:ps) = edad p: edades ps

promedioEn :: [Int] -> Int
promedioEn [] = 0
promedioEn ns = div (sumatoria ns) (longitud ns)

-- 1.3
elMasViejo :: [Persona] -> Persona
--Precondición: la lista al menos posee una persona.
elMasViejo [] = error "La lista dada no contiene personas"
elMasViejo [p] = p
elMasViejo (p:ps) = if edad p > edad (elMasViejo ps) 
                      then p
                      else elMasViejo ps

-- 2
data TipoDePokemon = Agua | Fuego | Planta
  deriving (Show)

data Pokemon = Pk TipoDePokemon Int --tipo energia
  deriving (Show)

data Entrenador = E String [Pokemon] --nombre lista de pk
  deriving (Show)

-- 2.1
cantPokemon :: Entrenador -> Int
cantPokemon (E _ ps) = longitud ps

-- 2.2
cantPokemonDe :: TipoDePokemon -> Entrenador -> Int
cantPokemonDe t (E _ ps) = longitud (soloDeTipo t ps)

soloDeTipo :: TipoDePokemon -> [Pokemon] -> [Pokemon]
soloDeTipo t [] = []
soloDeTipo t (p:ps) = if mismoTipo (tipo p) t
                        then p:soloDeTipo t ps
                        else soloDeTipo t ps

tipo :: Pokemon -> TipoDePokemon
tipo (Pk t _) = t

mismoTipo :: TipoDePokemon -> TipoDePokemon -> Bool
mismoTipo Agua Agua = True
mismoTipo Planta Planta = True
mismoTipo Fuego Fuego = True
mismoTipo _ _ = False

-- 2.3
cuantosDeTipo_De_LeGananATodosLosDe_:: TipoDePokemon -> Entrenador -> Entrenador -> Int
cuantosDeTipo_De_LeGananATodosLosDe_ t e1 e2 = cantPokemonDeTipo_De_QueGananA_ t (pokemonDe e1) (pokemonDe e2)

cantPokemonDeTipo_De_QueGananA_ :: TipoDePokemon -> [Pokemon] -> [Pokemon] -> Int
cantPokemonDeTipo_De_QueGananA_ t [] ps = 0
cantPokemonDeTipo_De_QueGananA_ t (p1:p1s) p2s = unoSiCeroSino(mismoTipo t (tipo p1) && superaATodos p1 p2s) + cantPokemonDeTipo_De_QueGananA_ t p1s p2s 

superaATodos :: Pokemon -> [Pokemon] -> Bool
superaATodos pk [] = True
superaATodos pk (p:ps) = superaA pk p && superaATodos pk ps

pokemonDe :: Entrenador -> [Pokemon]
pokemonDe (E _ ps) = ps

unoSiCeroSino :: Bool -> Int
unoSiCeroSino True = 1
unoSiCeroSino _ = 0

superaA :: Pokemon -> Pokemon -> Bool
superaA n m = venceA (tipo n, tipo m)

venceA :: (TipoDePokemon, TipoDePokemon) -> Bool
venceA (Agua, Fuego) = True
venceA (Fuego, Planta) = True
venceA (Planta, Agua) = True
venceA (_, _) = False

-- 2.4

esMaestroPokemon :: Entrenador -> Bool
esMaestroPokemon (E _ ps) = tieneTipo ps Fuego && tieneTipo ps Agua && tieneTipo ps Planta

tieneTipo :: [Pokemon] -> TipoDePokemon -> Bool
tieneTipo [] t = False
tieneTipo (p:ps) t = mismoTipo (tipo p) t || tieneTipo ps t

-- 3
data Seniority = Junior | SemiSenior | Senior
  deriving (Show)

data Proyecto = Py String --nombre
  deriving (Show)

data Rol = Developer Seniority Proyecto | Management Seniority Proyecto
  deriving (Show)

data Empresa = Em [Rol]
  deriving (Show)

-- 3.1
proyectos :: Empresa -> [Proyecto]
proyectos (Em rs) = proyectosDe rs
 
proyectosDe :: [Rol] -> [Proyecto]
proyectosDe [] = []
proyectosDe (r:rs) = if pertenece (nombreDelProyecto (proyectoDelRol r)) (nombresDeProyectos (proyectosDe rs))
                           then proyectosDe rs
                           else proyectoDelRol r : proyectosDe rs

proyectoDelRol :: Rol -> Proyecto
proyectoDelRol (Developer _ pr) = pr
proyectoDelRol (Management _ pr) = pr

nombresDeProyectos :: [Proyecto] -> [String]
nombresDeProyectos [] = []
nombresDeProyectos (p:ps) = if pertenece (nombreDelProyecto p) (nombresDeProyectos ps) 
                                then nombresDeProyectos ps
                                else nombreDelProyecto p:nombresDeProyectos ps

nombreDelProyecto :: Proyecto -> String
nombreDelProyecto (Py n) = n


-- 3.2
losDevSenior :: Empresa -> [Proyecto] -> Int
losDevSenior (Em rs) ps = cantidadDevSrDe_EnProyectos rs ps 

cantidadDevSrDe_EnProyectos :: [Rol] -> [Proyecto] -> Int
cantidadDevSrDe_EnProyectos [] _ = 0
cantidadDevSrDe_EnProyectos (r:rs) ps = unoSiCeroSino ( esDevYSr r && pertenece (nombreDelProyecto (proyectoDelRol r)) (nombresDeProyectos ps) ) + cantidadDevSrDe_EnProyectos rs ps 

esDevYSr :: Rol -> Bool
esDevYSr (Developer Senior _ ) = True
esDevYSr _ = False


-- 3.3
cantQueTrabajanEn :: [Proyecto] -> Empresa -> Int
cantQueTrabajanEn ps e = cantEmpleadosEnProyectosDe ps (rolesDe e)

rolesDe :: Empresa -> [Rol]
rolesDe (Em rs) = rs

cantEmpleadosEnProyectosDe :: [Proyecto] -> [Rol] -> Int
cantEmpleadosEnProyectosDe _ [] = 0
cantEmpleadosEnProyectosDe ps (r:rs) = unoSiCeroSino (pertenece (nombreDelProyecto (proyectoDelRol r)) (nombresDeProyectos ps)) + cantEmpleadosEnProyectosDe ps rs


-- 3.4
asignadosPorProyecto :: Empresa -> [(Proyecto, Int)]
asignadosPorProyecto (Em rs) = asignadosAProyectosDe rs

asignadosAProyectosDe :: [Rol] -> [(Proyecto, Int)]
asignadosAProyectosDe [] = []
asignadosAProyectosDe (r:rs) = if pertenece (nombreDelProyecto (proyectoDelRol r)) (nombresDeProyectos (proyectosDe rs))
                                then asignadosAProyectosDe rs
                                else (proyectoDelRol r, cantEmpleadosEnProyectoDe (proyectoDelRol r) rs) : asignadosAProyectosDe rs

cantEmpleadosEnProyectoDe :: Proyecto -> [Rol] -> Int
cantEmpleadosEnProyectoDe p [] = 0
cantEmpleadosEnProyectoDe p (r:rs) = unoSiCeroSino (mismoNombre (nombreDelProyecto (proyectoDelRol r)) (nombreDelProyecto p)) + cantEmpleadosEnProyectoDe p rs

mismoNombre :: String -> String -> Bool
mismoNombre s n = s == n 