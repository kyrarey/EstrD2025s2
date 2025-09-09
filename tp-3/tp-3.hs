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
  deriving (Show)

data Camino = Fin | Cofre [Objeto] Camino | Nada Camino
  deriving (Show)

--TEST
d1 = Cofre [Cacharro, Tesoro] (Nada (Cofre [Cacharro, Cacharro] Fin))
d2 = Nada ( Cofre [Cacharro, Cacharro] (Nada (Cofre [Cacharro, Cacharro] Fin)))


-- 1.2.1
hayTesoro :: Camino -> Bool
hayTesoro Fin = False
hayTesoro (Cofre objs c) = hayTesoroEnObjs objs || hayTesoro c
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
pasosHastaTesoro (Cofre objs c) = unoSi (not (hayTesoroEnObjs objs)) + pasosHastaTesoro c
pasosHastaTesoro (Nada c) = pasosHastaTesoro c

-- 1.2.3
hayTesoroEn :: Int -> Camino -> Bool
hayTesoroEn _ Fin = False
hayTesoroEn n (Cofre objs c) = if pasosHastaTesoro c /= n
                                  then hayTesoroEn (n-1) c
                                  else hayTesoroEnObjs objs 
hayTesoroEn n (Nada c) = hayTesoroEn n c

-- 1.2.4
alMenosNTesoros :: Int -> Camino -> Bool
alMenosNTesoros _ Fin = False
alMenosNTesoros n (Cofre objs c) = (numTotalDeTesoros objs c) > n || alMenosNTesoros n c
alMenosNTesoros n (Nada c) = alMenosNTesoros n c

numTotalDeTesoros :: [Objeto] -> Camino -> Int
numTotalDeTesoros _ Fin = 0
numTotalDeTesoros objs (Cofre objs1 c) = unoSi (hayTesoroEnObjs objs) + numTotalDeTesoros objs c
numTotalDeTesoros objs (Nada c) = numTotalDeTesoros objs c

-- 1.2.5
cantTesorosEntre :: Int -> Int -> Camino -> Int
cantTesorosEntre _ _ Fin = 0
cantTesorosEntre n1 n2 (Cofre objs c) = if numPasosEnCamino c >= n1 || numPasosEnCamino c <= n2
                                          then unoSi (hayTesoroEnObjs objs) + cantTesorosEntre n1 n2 c
                                          else cantTesorosEntre n1 n2 c
cantTesorosEntre n1 n2 (Nada c) = cantTesorosEntre n1 n2 c

numPasosEnCamino :: Camino -> Int
numPasosEnCamino Fin = 0
numPasosEnCamino (Cofre objs c) = unoSi (esFin c) + numPasosEnCamino c
numPasosEnCamino (Nada c) = numPasosEnCamino c

esFin :: Camino -> Bool
esFin Fin = True
esFin _ = False

-- 2 TIPOS ARBOREOS

-- 2.1. ARBOLES BINARIOS
data Tree a = EmptyT | NodeT a (Tree a) (Tree a)
  deriving (Show)

-- 2.1.1
sumarT :: Tree Int -> Int
sumarT EmptyT = 0
sumarT (NodeT x t1 t2) = x + sumarT t1 + sumarT t2 

-- 2.1.2
sizeT :: Tree a -> Int
sizeT EmptyT = 0
sizeT (NodeT x t1 t2) = 1 + sizeT t1 + sizeT t2 

-- 2.1.3
mapDobleT :: Tree Int -> Tree Int
mapDobleT EmptyT = EmptyT
mapDobleT (NodeT x t1 t2) = (NodeT (x*2) (mapDobleT t1) (mapDobleT t2) )

-- 2.1.4
perteneceT :: Eq a => a -> Tree a -> Bool
perteneceT _ EmptyT = False
perteneceT e (NodeT x t1 t2) = e==x || perteneceT e t1 || perteneceT e t2

-- 2.1.5
aparicionesT :: Eq a => a -> Tree a -> Int
aparicionesT _ EmptyT = 0
aparicionesT e (NodeT x t1 t2) = unoSi (e == x) + aparicionesT e t1 + aparicionesT e t2

-- 2.1.6
leaves :: Tree a -> [a]
leaves EmptyT = []
leaves (NodeT x t1 t2) = siEsHoja (NodeT x t1 t2) ++ leaves t1 ++ leaves t2

siEsHoja :: Tree a -> [a]
siEsHoja (NodeT x EmptyT EmptyT) = [x]
siEsHoja _ = []

