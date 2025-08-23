-- 2.1
-- A
sucesor :: Int -> Int
sucesor n = n + 1

-- B
sumar :: Int -> Int -> Int
sumar n m = n + m

-- C
divisionYResto :: Int -> Int -> (Int, Int)
divisionYResto n m = (div n m, mod n m)

-- D
maxDelPar :: (Int, Int) -> Int
maxDelPar (n, m) =
  if (n > m)
    then n
    else m

{-
-- 2.2
        sumar (maxDelPar (divisiónYResto 18 2)) (sucesor 0)
        sucesor (sumar (maxDelPar (divisiónYResto 6 3)) 7)
        sumar (sucesor (maxDelPar (divisiónYResto 180 20))) 0
        maxDelPar(divisiónYResto (sumar 12 8) (sucesor 1))
-}

-- 3.1
data Dir = Norte | Este | Sur | Oeste
  deriving (Show)

-- A
opuesto :: Dir -> Dir
opuesto Norte = Sur
opuesto Este = Oeste
opuesto Sur = Norte
opuesto Oeste = Este

-- B
iguales :: Dir -> Dir -> Bool
iguales Norte Norte = True
iguales Este Este = True
iguales Sur Sur = True
iguales Oeste Oeste = True
iguales _ _ = False

-- C
siguiente :: Dir -> Dir
-- Precondición: La dirección ingresada no es Oeste
siguiente Norte = Este
siguiente Este = Sur
siguiente Sur = Oeste

-- Es una función parcial, va a fallar si la dirección agregada es Oeste.

-- 3.2
data DiaDeSemana = Lunes | Martes | Miercoles | Jueves | Viernes | Sabado | Domingo
  deriving (Show)

-- A
primeroYUltimoDia :: (DiaDeSemana, DiaDeSemana)
primeroYUltimoDia = (primerDia, ultimoDia)

primerDia :: DiaDeSemana
primerDia = Lunes

ultimoDia :: DiaDeSemana
ultimoDia = Domingo

-- B
empiezaConM :: DiaDeSemana -> Bool
empiezaConM Martes = True
empiezaConM Miercoles = True
empiezaConM _ = False

-- C
vieneDespues :: DiaDeSemana -> DiaDeSemana -> Bool
vieneDespues x y = valorDelDia x > valorDelDia y

valorDelDia :: DiaDeSemana -> Int
valorDelDia Lunes = 1
valorDelDia Martes = 2
valorDelDia Miercoles = 3
valorDelDia Jueves = 4
valorDelDia Viernes = 5
valorDelDia Sabado = 6
valorDelDia Domingo = 7

-- D
estaEnElMedio :: DiaDeSemana -> Bool
estaEnElMedio Lunes = False
estaEnElMedio Domingo = False
estaEnElMedio _ = True

-- 3.3
-- A
negar :: Bool -> Bool
negar True = False
negar _ = True

-- B
implica :: Bool -> Bool -> Bool
implica True n = n
implica False _ = True

-- C
yTambien :: Bool -> Bool -> Bool
yTambien True n = n
yTambien False _ = False

-- D
oBien :: Bool -> Bool -> Bool
oBien True _ = True
oBien False n = n

-- 4.1
data Persona = P String Int
  deriving (Show)

-- Nombre Edad

-- A
nombre :: Persona -> String
nombre (P n e) = n

-- B
edad :: Persona -> Int
edad (P n e) = e

-- C
crecer :: Persona -> Persona -- AAAA
crecer (P n e) = P n (sucesor e)

-- D
cambioDeNombre :: String -> Persona -> Persona -- AAAA
cambioDeNombre m (P n e) = P m e

-- E
esMayorQueLaOtra :: Persona -> Persona -> Bool
esMayorQueLaOtra p1 p2 = edad p1 > edad p2

-- F
laQueEsMayor :: Persona -> Persona -> Persona -- AAAA
laQueEsMayor p1 p2 =
  if edad p1 > edad p2
    then p1
    else p2

-- 4.2
data TipoDePokemon = Agua | Fuego | Planta
  deriving (Show)

data Pokemon = M TipoDePokemon Int
  deriving (Show)

-- Tipo          Energía

data Entrenador = E String Pokemon Pokemon
  deriving (Show)

-- Nombre Pokemon Pokemon

-- A
superaA :: Pokemon -> Pokemon -> Bool
superaA n m = venceA (tipo n, tipo m)

venceA :: (TipoDePokemon, TipoDePokemon) -> Bool
venceA (Agua, Fuego) = True
venceA (Fuego, Planta) = True
venceA (Planta, Agua) = True
venceA (_, _) = False

tipo :: Pokemon -> TipoDePokemon
tipo (M t e) = t

-- B
cantidadDePokemonDe :: TipoDePokemon -> Entrenador -> Int
cantidadDePokemonDe t (E _ p1 p2) = unoSiCeroSino (coincidenTipos (t, tipo p1)) + unoSiCeroSino (coincidenTipos (t, tipo p2))

unoSiCeroSino :: Bool -> Int
unoSiCeroSino True = 1
unoSiCeroSino _ = 0

coincidenTipos :: (TipoDePokemon, TipoDePokemon) -> Bool
coincidenTipos (Agua, Agua) = True
coincidenTipos (Planta, Planta) = True
coincidenTipos (Fuego, Fuego) = True
coincidenTipos (_, _) = False

-- C
juntarPokemon :: (Entrenador, Entrenador) -> [Pokemon] -- AAAA
-- Dado un par de entrenadores, devuelve a sus Pokémon en una lista.
primerPokemon :: Entrenador -> Pokemon
primerPokemon (E n p1 p2) = p1

segundoPokemon :: Entrenador -> Pokemon
segundoPokemon (E n p1 p2) = p2

juntarPokemon (e1, e2) = primerPokemon e1 : segundoPokemon e1 : primerPokemon e2 : segundoPokemon e2 : []

-- 5.1

-- A
loMismo :: a -> a
loMismo x = x

-- B
siempreSiete :: a -> Int
siempreSiete x = 7

-- C
swap :: (a, b) -> (b, a)
swap (x, y) = (y, x)

-- 5.2
-- Porque son funciones genéricas cuyos datos específicos no son relevantes.

-- 6.1

-- A
estaVacia :: [a] -> Bool
estaVacia [] = True
estaVacia _ = False

-- B
elPrimero :: [a] -> a
elPrimero (x : _) = x

-- C
sinElPrimero :: [a] -> [a]
sinElPrimero (_ : xs) = xs

-- D
splitHead :: [a] -> (a, [a])
splitHead (x : xs) = (x, xs)