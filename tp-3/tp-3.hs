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
poner c1 (Bolita c2 cel) = Bolita c1 (Bolita c2 cel)

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
ponerN n c (Bolita c2 cel) = ponerN (n - 1) c (poner c cel)

-- 1.2 CAMINO HACIA EL TESORO
data Objeto = Cacharro | Tesoro
  deriving (Show)

data Camino = Fin | Cofre [Objeto] Camino | Nada Camino
  deriving (Show)

--TEST
c1 = Cofre [Cacharro, Tesoro] 
        (Nada 
            (Cofre [Cacharro, Cacharro] Fin)
            )
c2 = Nada ( 
        Cofre [Cacharro, Cacharro] 
            (Nada 
                (Cofre [Cacharro, Cacharro] Fin))
          )

c3 = Nada (
          Cofre [Tesoro, Cacharro] 
              (Nada 
                  (Cofre [Tesoro, Tesoro] Fin))
          )

c4 = Cofre [Cacharro] 
        (Nada 
            (Cofre [Cacharro, Cacharro] 
                (Cofre [Cacharro, Tesoro] Fin))
        )


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
-- pasosHastaTesoro Fin = nunca se llega a fin porque se encuentra el tesoro antes
pasosHastaTesoro (Cofre objs c) = if hayTesoroEnObjs objs
                                    then 0
                                    else 1 + pasosHastaTesoro c
pasosHastaTesoro (Nada c) = 1 + pasosHastaTesoro c

-- 1.2.3
hayTesoroEn :: Int -> Camino -> Bool
hayTesoroEn _ Fin = False
hayTesoroEn n (Cofre objs c) = if pasosHastaTesoro c /= n
                                  then hayTesoroEn (n-1) c
                                  else hayTesoroEnObjs objs 
hayTesoroEn n (Nada c) = hayTesoroEn n c

-- 1.2.4
alMenosNTesoros :: Int -> Camino -> Bool
alMenosNTesoros n c = (cantTesorosEnCamino c ) >= n

cantTesorosEnCamino :: Camino -> Int
cantTesorosEnCamino Fin = 0
cantTesorosEnCamino (Cofre objs c) = cantTesorosEnObjs objs + cantTesorosEnCamino c
cantTesorosEnCamino (Nada c) = cantTesorosEnCamino c

cantTesorosEnObjs :: [Objeto] -> Int
cantTesorosEnObjs [] = 0
cantTesorosEnObjs (obj:objs) = unoSi (esTesoro obj) + cantTesorosEnObjs objs

-- 1.2.5 !!
cantTesorosEntre :: Int -> Int -> Camino -> Int
cantTesorosEntre _ _ Fin = 0
cantTesorosEntre n1 n2 (Cofre objs c) = if numPasosEnCamino c >= n1 || numPasosEnCamino c <= n2
                                          then cantTesorosEnObjs objs + cantTesorosEntre n1 n2 c
                                          else cantTesorosEntre n1 n2 c
cantTesorosEntre n1 n2 (Nada c) = cantTesorosEntre n1 n2 c

numPasosEnCamino :: Camino -> Int
numPasosEnCamino Fin = 0
numPasosEnCamino (Cofre objs c) = unoSi (noEsFinal c) + numPasosEnCamino c
numPasosEnCamino (Nada c) = numPasosEnCamino c

noEsFinal :: Camino -> Bool
noEsFinal Fin = False
noEsFinal _ = True

-- 2 TIPOS ARBOREOS

-- 2.1. ARBOLES BINARIOS
data Tree a = EmptyT | NodeT a (Tree a) (Tree a)
  deriving (Show)

t1 :: Tree Int
t1 = NodeT 11 (NodeT 22 EmptyT EmptyT) (NodeT 37 EmptyT EmptyT)
t2 :: Tree Int
t2 = NodeT 1 (NodeT 1 EmptyT EmptyT) (NodeT 2 EmptyT EmptyT)
t3 :: Tree Int
t3 = NodeT 12 (NodeT 11 EmptyT 
                  (NodeT 45 EmptyT EmptyT)
              )
              (NodeT 2 EmptyT
                  (NodeT 23 EmptyT
                      (NodeT 45 EmptyT EmptyT))
              )
t4 :: Tree Int
t4 = NodeT 56 (NodeT 8 EmptyT 
                  (NodeT 290 EmptyT
                      (NodeT 76 EmptyT
                        (NodeT 46 EmptyT EmptyT)))
              )
              (NodeT 72 EmptyT
                  (NodeT 509 EmptyT
                      (NodeT 98 EmptyT EmptyT))
              )

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

-- 2.1.7
heightT :: Tree a -> Int
heightT EmptyT = 0
heightT (NodeT x t1 t2) = if sizeT t1 > sizeT t2
                              then unoSi (not (esHoja t1)) + heightT t1
                              else unoSi (not (esHoja t2)) + heightT t2
                
esHoja :: Tree a -> Bool
esHoja EmptyT = True
esHoja _ = False

-- 2.1.8
mirrorT :: Tree a -> Tree a
mirrorT EmptyT = EmptyT
mirrorT (NodeT x t1 t2) = NodeT x (mirrorT t2) (mirrorT t1)

-- 2.1.9
toList :: Tree a -> [a]
toList EmptyT = []
toList (NodeT x t1 t2) = toList t1 ++ [x] ++ toList t2

-- 2.1.10
levelN :: Int -> Tree a -> [a]
levelN 0 (NodeT x t1 t2) = [x]
levelN _ EmptyT = []
levelN n (NodeT x t1 t2) = levelN (n-1) t1 ++ levelN (n-1) t2

-- 2.1.11
-- listPerLevel :: Tree a -> [[a]]  
-- listPerLevel EmptyT =
-- listPerLevel (NodeT x t1 t2) = x listPerLevel t1 listPerLevel t2 

-- 2.1.12
ramaMasLarga :: Tree a -> [a]
ramaMasLarga EmptyT = []
ramaMasLarga (NodeT x t1 t2) = if sizeT t1> sizeT t2
                                  then x:ramaMasLarga t1
                                  else x:ramaMasLarga t2 --si son del mismo largo devuelve la rama derecha

-- 2.1.13
-- todosLosCaminos :: Tree a -> [[a]]
-- todosLosCaminos EmptyT = []
-- todosLosCaminos (NodeT x t1 t2) = 
--   [x: maximal t1] ++ todosLosCaminos t1 ++ [x: maximal t2] ++ todosLosCaminos t2

-- maximal :: Tree a -> [a]
-- maximal EmptyT = []
-- maximal (NodeT x t1 t2) = [x]