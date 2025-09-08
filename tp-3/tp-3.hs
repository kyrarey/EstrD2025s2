-- PRACTICA 3

-- 1 TIPOS RECURSIVOS SIMPLES

-- 1.1 CELDAS CON BOLITAS
data Color = Azul | Rojo
  deriving (Show)
data Celda = Bolita Color Celda | CeldaVacia
  deriving (Show)

-- 1.1.1
nroBolitas :: Color -> Celda -> Int
nroBolitas _ CeldaVacia = 0
nroBolitas c1 (Bolita c2 cel) = unoSi (sonMismoColor c1 c2) + nroBolitas c1 cel

sonMismoColor :: Color -> Color -> Bool
sonMismoColor Azul Azul = True
sonMismoColor Rojo Rojo = True
sonMismoColor _ _ = False

unoSi :: Bool -> Int
unoSi True = 1
unoSi False = 0

-- 1.1.2
poner :: Color -> Celda -> Celda
poner c1 (Bolita c2 cel) = Bolita c1 (Bolita c2 CeldaVacia)

-- 1.1.3
sacar :: Color -> Celda -> Celda
sacar _ CeldaVacia = CeldaVacia
sacar c1 (Bolita c2 cel) =
  if (sonMismoColor c1 c2)
    then cel
    else Bolita c2 (sacar c1 cel)

-- 1.1.4
ponerN :: Int -> Color -> Celda -> Celda
ponerN 0 _ cel = cel
ponerN n c (Bolita c2 cel) =
  if n /= 1
    then ponerN (n - 1) c (poner c cel)
    else poner c cel

-- 1.2 CAMINO HACIA EL TESORO
data Objeto = Cacharro | Tesoro
  deriving (Eq)

data Camino = Fin | Cofre [Objeto] Camino | Nada Camino
  deriving (Show)

-- 1.2.1
hayTesoro :: Camino -> Bool
hayTesoro Fin = False
hayTesoro (Cofre objs c) = if hayTesoroEnObjs objs
                              then True
                              else hayTesoro c
hayTesoro (Nada c) = hayTesoro c

hayTesoroEnObjs :: [Objeto] -> Bool
hayTesoroEnObjs [] = False
hayTesoroEnObjs (obj:objs) = esTesoro obj || hayTesoroEnObjs objs

esTesoro :: Objeto -> Bool
esTesoro Tesoro = True
esTesoro _ = False

-- 1.2.2
pasosHastaTesoro :: Camino -> Int
-- Precondición: tiene que haber al menos un tesoro.
pasosHastaTesoro Fin = 0
pasosHastaTesoro (Cofre objs c) = unoSi (not hayTesoroEnObjs objs) + pasosHastaTesoro c
pasosHastaTesoro (Nada c) = pasosHastaTesoro c

-- 1.2.3 !
hayTesoroEn :: Int -> Camino -> Bool
hayTesoroEn _ Fin = 0
hayTesoroEn n (Cofre objs c) = if pasosHastaTesoro c /= n
                                  then hayTesoroEn n-1 c
                                  else hayTesoroEnObjs objs 
hayTesoroEn _ (Nada c) = hayTesoroEn c

-- 1.2.4
alMenosNTesoros :: Int -> Camino -> Bool
alMenosNTesoros _ Fin = False
--alMenosNTesoros n (Cofre objs c) = n objs alMenosNTesoros c
alMenosNTesoros _ (Nada c) = alMenosNTesoros c