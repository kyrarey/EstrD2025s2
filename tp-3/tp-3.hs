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
poner c b = Bolita c b

-- 1.1.3
sacar :: Color -> Celda -> Celda
sacar _ CeldaVacia = CeldaVacia
sacar c1 (Bolita c2 cel) =
  if (sonMismoColor c1 c2)
    then cel
    else Bolita c2 (sacar c1 cel)

-- 1.1.4
ponerN :: Int -> Color -> Celda -> Celda
ponerN 0 _ b = b
ponerN n c b = Bolita c (ponerN (n-1) c b)

-- 1.2 CAMINO HACIA EL TESORO
data Objeto = Cacharro | Tesoro
  deriving (Show)

data Camino = Fin | Cofre [Objeto] Camino | Nada Camino
  deriving (Show)

--TEST
-- c1 = Cofre [Cacharro, Tesoro] 
--         (Nada 
--             (Cofre [Cacharro, Cacharro] Fin)
--             )
-- c2 = Nada ( 
--         Cofre [Cacharro, Cacharro] 
--             (Nada 
--                 (Cofre [Cacharro, Cacharro] Fin))
--           )

-- c3 = Nada (
--           Cofre [Tesoro, Cacharro] 
--               (Nada 
--                   (Cofre [Tesoro, Tesoro] Fin))
--           )

-- c4 = Cofre [Cacharro] 
--         (Nada 
--             (Cofre [Cacharro, Cacharro] 
--                 (Cofre [Cacharro, Tesoro] Fin))
--         )


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
hayTesoroEn 0 (Cofre objs c) = hayTesoroEnObjs objs
hayTesoroEn n Fin          = False
hayTesoroEn n (Cofre objs c) = hayTesoroEn (n-1) c
hayTesoroEn n (Nada c)     = hayTesoroEn (n-1) c

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
cantTesorosEntre :: Int -> Int -> Camino -> Int --Precondición: n1 no es mayor que n2.
cantTesorosEntre _ _ Fin        = 0
cantTesorosEntre 0 0 c          = cantTesorosSiHay c
cantTesorosEntre 0 n2 c         = cantTesorosSiHay c + cantTesorosEntre 0 (n2-1) (avanzar c)
cantTesorosEntre n1 n2 c        = cantTesorosEntre (n1-1) (n2-1) (avanzar c)

cantTesorosSiHay :: Camino -> Int
cantTesorosSiHay (Cofre x _ ) = cantTesorosEnObjs x
cantTesorosSiHay _            = 0

avanzar :: Camino -> Camino --Precondición: No es el fin del camino.
avanzar Fin = error "No se puede avanzar."
avanzar (Cofre _ c) = c
avanzar (Nada c)    = c

-- 2 TIPOS ARBOREOS

-- 2.1. ARBOLES BINARIOS
data Tree a = EmptyT | NodeT a (Tree a) (Tree a)
  deriving (Show)

-- t1 :: Tree Int
-- t1 = NodeT 11 (NodeT 22 EmptyT EmptyT) (NodeT 37 EmptyT EmptyT)
-- t2 :: Tree Int
-- t2 = NodeT 1 (NodeT 1 EmptyT EmptyT) (NodeT 2 EmptyT EmptyT)
-- t3 :: Tree Int
-- t3 = NodeT 12 (NodeT 11 EmptyT 
--                   (NodeT 45 EmptyT EmptyT)
--               )
--               (NodeT 2 EmptyT
--                   (NodeT 23 EmptyT
--                       (NodeT 45 EmptyT EmptyT))
--               )
-- t4 :: Tree Int
-- t4 = NodeT 56 (NodeT 8 EmptyT 
--                   (NodeT 290 EmptyT
--                       (NodeT 76 EmptyT
--                         (NodeT 46 EmptyT EmptyT)))
--               )
--               (NodeT 72 EmptyT
--                   (NodeT 509 EmptyT
--                       (NodeT 98 EmptyT EmptyT))
--               )

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
heightT (NodeT x t1 t2) = 1 + elMasGrande (heightT t1) (heightT t2)

elMasGrande :: Int -> Int -> Int
elMasGrande n n1 = if n < n1
                    then n1
                    else n
                
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
listPerLevel :: Tree a -> [[a]]
listPerLevel EmptyT = []
listPerLevel (NodeT x t1 t2) = [x] : juntarNiveles (listPerLevel t1) (listPerLevel t2)

juntarNiveles :: [[a]] -> [[a]] -> [[a]]
juntarNiveles xs [] = xs
juntarNiveles [] ys = ys
juntarNiveles (xs:xss) (ys:yss) = (xs ++ ys) : juntarNiveles xss yss

-- 2.1.12
ramaMasLarga :: Tree a -> [a]
ramaMasLarga EmptyT = []
ramaMasLarga (NodeT x t1 t2) = x : laMasLarga (ramaMasLarga t1) (ramaMasLarga t2)

laMasLarga :: [a] -> [a] -> [a]
laMasLarga x y = if length x > length y
                  then x
                  else y

-- 2.1.13
todosLosCaminos :: Tree a -> [[a]]
todosLosCaminos EmptyT = []
todosLosCaminos (NodeT x t1 t2) = [x] : (consA x (todosLosCaminos t1)) ++ (consA x (todosLosCaminos t2))

consA :: a -> [[a]] -> [[a]]
consA _ [] = []
consA y (xs:xss) = (y: xs) : (consA y xss)


--2.2 EXPRESIONES ARITMETICAS
data ExpA = Valor Int | Sum ExpA ExpA | Prod ExpA ExpA | Neg ExpA
   deriving Show

-- 2.2.1
eval :: ExpA -> Int
eval (Valor n)    = n
eval (Sum n1 n2)  = eval n1 + eval n2
eval (Prod n1 n2) = eval n1 * eval n2
eval (Neg n) = -(eval n)

-- 2.2.2
simplificar :: ExpA -> ExpA
simplificar (Valor n)          = Valor n
simplificar (Sum (Valor 0) n)  = simplificar n
simplificar (Sum n (Valor 0))  = simplificar n
simplificar (Prod (Valor 0) n) = Valor 0
simplificar (Prod n (Valor 0)) = Valor 0
simplificar (Prod (Valor 1) n) = simplificar n
simplificar (Prod n (Valor 1)) = simplificar n
simplificar (Neg (Neg n))      = simplificar n
simplificar (Sum n1 n2)        = (Sum (simplificar n1) (simplificar n2))
simplificar (Prod n1 n2)       = (Prod (simplificar n1) (simplificar n2))
simplificar (Neg n)            = (Neg (simplificar n))