-- PRACTICA 4

-- 1 PIZZAS
data Pizza = Prepizza | Capa Ingrediente Pizza
  deriving (Show)
data Ingrediente = Salsa | Queso | Jamon | Aceitunas Int
  deriving (Show)

p1 = Capa Salsa (Capa Queso Prepizza)
p2 = Capa Salsa (Capa Queso (Capa Jamon Prepizza))
p3 = Capa Salsa (Capa Queso (Capa Jamon (Capa Jamon Prepizza)))
p4 = Capa Salsa (Capa Queso (Capa Jamon (Capa (Aceitunas 4) Prepizza)))


--1.1
cantidadDeCapas :: Pizza -> Int
cantidadDeCapas Prepizza = 0
cantidadDeCapas (Capa ing p) = 1 + cantidadDeCapas p

esPrepizza :: Pizza -> Bool
esPrepizza Prepizza = True
esPrepizza _ = False

--1.2
armarPizza :: [Ingrediente] -> Pizza
armarPizza [] = Prepizza
armarPizza (ing: ings) = Capa ing (armarPizza ings)

--1.3
sacarJamon :: Pizza -> Pizza
sacarJamon Prepizza = Prepizza
sacarJamon (Capa ing p) = if (esIngrediente Jamon ing)
                            then (sacarJamon p)
                            else Capa ing (sacarJamon p)

esIngrediente :: Ingrediente -> Ingrediente -> Bool
esIngrediente Salsa Salsa = True
esIngrediente Queso Queso = True
esIngrediente Jamon Jamon = True
esIngrediente (Aceitunas _) (Aceitunas _) = True
esIngrediente _ _ = False

--1.4
tieneSoloSalsaYQueso :: Pizza -> Bool
tieneSoloSalsaYQueso Prepizza = True
tieneSoloSalsaYQueso (Capa ing p) = ((esIngrediente Salsa ing) || (esIngrediente Queso ing)) && tieneSoloSalsaYQueso p

--1.5
--duplicarAceitunas :: Pizza -> Pizza
--duplicarAceitunas Prepizza = Prepizza
--duplicarAceitunas (Capa ing p) = if esIngrediente ing (Aceitunas _)
--                                   then Capa ( masAceitunas ing ) (duplicarAceitunas p) -- porque no (Aceitunas (n * 2)) ?
--                                   else Capa ing (duplicarAceitunas p)

--masAceitunas :: Ingrediente -> Ingrediente
-- precon: el ing solo pueden ser aceitunas
--masAceitunas (Aceitunas n ) = Aceitunas (n*2)
--masAceitunas _ = error "Solo se pueden multiplicar aceitunas"


--1.6
cantCapasPorPizza :: [Pizza] -> [(Int, Pizza)]
cantCapasPorPizza [] = []
cantCapasPorPizza (p:ps) = (cantidadDeCapas p,p): cantCapasPorPizza ps



--2 MAPA DE TESOROS
data Dir = Izq | Der
  deriving (Show)
data Objeto = Tesoro | Chatarra
  deriving (Show)
data Cofre = Cofre [Objeto]
  deriving (Show)
data Mapa = Fin Cofre | Bifurcacion Cofre Mapa Mapa
  deriving (Show)

m1 =  Bifurcacion
        (Cofre [Chatarra, Chatarra] ) 
        (Bifurcacion 
            (Cofre [Chatarra, Chatarra])
            (Fin (Cofre [Chatarra, Chatarra] ))
            (Fin (Cofre [Chatarra, Tesoro] ))
        ) 
        (Fin (Cofre [Chatarra, Chatarra] ))

m2 =  Bifurcacion
        (Cofre [Chatarra, Chatarra] ) 
        (Bifurcacion 
            (Cofre [Chatarra, Chatarra])
            (Fin (Cofre [Chatarra, Chatarra] ))
            (Fin (Cofre [Chatarra, Chatarra] ))
        ) 
        (Fin (Cofre [Chatarra, Chatarra] ))

m3 = Bifurcacion (Cofre [Chatarra, Chatarra]) (Fin (Cofre [Chatarra, Chatarra])) (Fin (Cofre [Chatarra, Chatarra]))

--2.1
hayTesoro :: Mapa -> Bool
hayTesoro (Fin c) = objsEsTesoro (abrirCofre c)
hayTesoro (Bifurcacion c m1 m2) = objsEsTesoro (abrirCofre c) || (hayTesoro m1) || (hayTesoro m2)

abrirCofre :: Cofre -> [Objeto]
abrirCofre (Cofre objs) = objs

objsEsTesoro :: [Objeto] -> Bool
objsEsTesoro [] = False
objsEsTesoro (obj:objs) = (esTesoro obj) || (objsEsTesoro objs)

esTesoro :: Objeto -> Bool
esTesoro Tesoro = True
esTesoro _ = False

--2.2
hayTesoroEn :: [Dir] -> Mapa -> Bool
hayTesoroEn [] (Fin c) = objsEsTesoro (abrirCofre c)
hayTesoroEn [] (Bifurcacion c m1 m2) = objsEsTesoro (abrirCofre c)
hayTesoroEn (d:ds) (Fin c) = False
hayTesoroEn (d:ds) (Bifurcacion c m1 m2) =  if esIzq d
                                              then hayTesoroEn ds m1 
                                              else hayTesoroEn ds m2

esIzq :: Dir -> Bool
esIzq Izq = True
esIzq _ = False

--2.3
caminoAlTesoro :: Mapa -> [Dir]
--Precondición: existe un tesoro y es único.
caminoAlTesoro (Fin c) = []
caminoAlTesoro (Bifurcacion c m1 m2) = if objsEsTesoro (abrirCofre c)
                                          then [] 
                                          else if hayTesoro m1
                                            then Izq:caminoAlTesoro m1
                                            else Der:caminoAlTesoro m2

--2.4
caminoDeLaRamaMasLarga :: Mapa -> [Dir]
caminoDeLaRamaMasLarga (Fin c) = []
caminoDeLaRamaMasLarga (Bifurcacion c m1 m2) = if (largoDeRama m1) > (largoDeRama m2)
                                                  then Izq:caminoDeLaRamaMasLarga m1
                                                  else Der:caminoDeLaRamaMasLarga m2

largoDeRama :: Mapa -> Int 
largoDeRama (Fin c) = 0
largoDeRama (Bifurcacion c m1 m2) = 1 + largoDeRama m1 + largoDeRama m2

--2.5
tesorosPorNivel :: Mapa -> [[Objeto]]
tesorosPorNivel (Fin c) = [listaDeTesorosSiHay (abrirCofre c)]
tesorosPorNivel (Bifurcacion c m1 m2) = listaDeTesorosSiHay (abrirCofre c) : juntarNiveles (tesorosPorNivel m1) (tesorosPorNivel m2)
                                          
listaDeTesorosSiHay :: [Objeto] -> [Objeto] -- se podra generalizar?
listaDeTesorosSiHay (obj: objs) = if (esTesoro obj)
                                    then obj: (listaDeTesorosSiHay objs)
                                    else (listaDeTesorosSiHay objs)
listaDeTesorosSiHay _ = []

juntarNiveles :: [[a]] -> [[a]] -> [[a]]
juntarNiveles xs [] = xs
juntarNiveles [] ys = ys
juntarNiveles (xs:xss) (ys:yss) = (xs ++ ys) : juntarNiveles xss yss

--2.6
-- todosLosCaminos :: Mapa -> [[Dir]]
-- todosLosCaminos (Fin c) = []
-- todosLosCaminos (Bifurcacion c m1 m2) =  todosLosCaminos m1 todosLosCaminos m2

--Devuelve todos lo caminos en el mapa.

-- 3 NAVE ESPACIAL
data Componente = LanzaTorpedos | Motor Int | Almacen [Barril]
  deriving (Show)
data Barril = Comida | Oxigeno | Torpedo | Combustible
  deriving (Show)
data Sector = S SectorId [Componente] [Tripulante]
  deriving (Show)
type SectorId = String
type Tripulante = String
data Tree a = EmptyT | NodeT a (Tree a) (Tree a)
  deriving (Show)
data Nave = N (Tree Sector)
  deriving (Show)

n1 = N (NodeT (S "1353" [LanzaTorpedos, Motor 3] ["M", "K", "D", "S"]) EmptyT EmptyT )

n2 = N (NodeT (S "3006" [Almacen [Comida, Torpedo]] ["M", "K", "D", "S"]) EmptyT EmptyT)

n3 = N (NodeT (S "8972" [Almacen [Comida, Torpedo]] ["M", "K", "D", "S"]) EmptyT (NodeT (S "4567" [Motor 6, LanzaTorpedos] ["M", "S"]) EmptyT EmptyT))


-- 3.1
sectores :: Nave -> [SectorId]
sectores (N t) = sectoresDelArbol t

sectoresDelArbol :: Tree Sector -> [SectorId]
sectoresDelArbol EmptyT = []
sectoresDelArbol (NodeT s t1 t2) = (idSector s : sectoresDelArbol t1) ++ sectoresDelArbol t2

idSector :: Sector -> SectorId
idSector (S sId _ _) = sId

-- Propósito: Devuelve todos los sectores de la nave.

-- 3.2
poderDePropulsion :: Nave -> Int
poderDePropulsion (N t) = poderDePropulsionDelArbol t

poderDePropulsionDelArbol :: Tree Sector -> Int
poderDePropulsionDelArbol EmptyT = 0
poderDePropulsionDelArbol (NodeT s t1 t2) = poderDePropulsionDelSector s + poderDePropulsionDelArbol t1 + poderDePropulsionDelArbol t2

poderDePropulsionDelSector :: Sector -> Int
poderDePropulsionDelSector (S _ cs _) =  propulsionDeMotorSiHay cs 

propulsionDeMotorSiHay :: [Componente] -> Int
propulsionDeMotorSiHay [] = 0
propulsionDeMotorSiHay (c:cs) = propulsionSiEsMotorCeroSino c + propulsionDeMotorSiHay cs

-- esMotor :: Componente -> Bool
-- esMotor (Motor _) = True
-- esMotor _ = False

propulsionSiEsMotorCeroSino :: Componente -> Int
propulsionSiEsMotorCeroSino (Motor n) = n
propulsionSiEsMotorCeroSino _ = 0

-- Propósito: Devuelve la suma de poder de propulsión de todos los motores de la nave. Nota:
-- el poder de propulsión es el número que acompaña al constructor de motores.

--asignados por proyecto (tp 2), 

-- 3.3
barriles :: Nave -> [Barril]
barriles (N t) = barrilesDelArbol t

barrilesDelArbol :: Tree Sector -> [Barril]
barrilesDelArbol EmptyT = []
barrilesDelArbol (NodeT s t1 t2) = barrilesDelSector s ++ barrilesDelArbol t1 ++ barrilesDelArbol t2
-- Propósito: Devuelve todos los barriles de la nave.

barrilesDelSector :: Sector -> [Barril]
barrilesDelSector (S _ cs _) = barrilesDeComponente cs

barrilesDeComponente :: [Componente] -> [Barril]
barrilesDeComponente [] = []
barrilesDeComponente (c:cs) = if esAlmacen c 
                                then darBarril c ++ barrilesDeComponente cs
                                else barrilesDeComponente cs

esAlmacen :: Componente -> Bool
esAlmacen (Almacen _ ) = True
esAlmacen _ = False

darBarril :: Componente -> [Barril]
darBarril (Almacen bs) = bs
darBarril _ = []

-- 3.4
agregarASector :: [Componente] -> SectorId -> Nave -> Nave
agregarASector cs sID (N t) = (N (agregarASectorDelArbol cs sID t))

agregarASectorDelArbol :: [Componente] -> SectorId -> Tree Sector -> Tree Sector
agregarASectorDelArbol cs sID EmptyT = EmptyT
agregarASectorDelArbol cs sID (NodeT s t1 t2) = if (idSector s == sID )
                                                  then (NodeT (agregarComponenteASector cs s) t1 t2) 
                                                  else (NodeT s (agregarASectorDelArbol cs sID t1) (agregarASectorDelArbol cs sID t2))

agregarComponenteASector :: [Componente] -> Sector -> Sector
agregarComponenteASector cs (S sID cs1 ts) = (S sID (cs1++cs) ts)

-- Propósito: Añade una lista de componentes a un sector de la nave.
-- Nota: ese sector puede no existir, en cuyo caso no añade componentes.

-- 3.5
asignarTripulanteA :: Tripulante -> [SectorId] -> Nave -> Nave
asignarTripulanteA tr sIDs (N t) = (N (asignarTripulanteADelArbol tr sIDs t))

asignarTripulanteADelArbol :: Tripulante -> [SectorId] -> Tree Sector -> Tree Sector
asignarTripulanteADelArbol tr sIDs EmptyT = EmptyT
asignarTripulanteADelArbol tr (sID:sIDs) (NodeT s t1 t2) = if idSector s == sID
                                                            then (NodeT (asignarTripulanteASector tr s) t1 t2)
                                                            else (NodeT s (asignarTripulanteADelArbol tr sIDs t1) (asignarTripulanteADelArbol tr sIDs t2) )

asignarTripulanteASector :: Tripulante -> Sector -> Sector
asignarTripulanteASector tr (S sID cs trs) = (S sID cs (tr:trs))

-- Propósito: Incorpora un tripulante a una lista de sectores de la nave.
-- Precondición: Todos los id de la lista existen en la nave.

-- 3.6
sectoresAsignados :: Tripulante -> Nave -> [SectorId]
sectoresAsignados tr (N t) = sectoresAsignadosDelArbol tr t

sectoresAsignadosDelArbol :: Tripulante -> Tree Sector -> [SectorId]
sectoresAsignadosDelArbol tr EmptyT = []
sectoresAsignadosDelArbol tr (NodeT s t1 t2) = if hayTripulanteEnSector tr s
                                                 then (idSector s:sectoresAsignadosDelArbol tr t1) ++ sectoresAsignadosDelArbol tr t2
                                                 else sectoresAsignadosDelArbol tr t1 ++ sectoresAsignadosDelArbol tr t2

hayTripulanteEnSector :: Tripulante -> Sector -> Bool
hayTripulanteEnSector tr1 (S _ _ (tr : trs)) = hayTripulanteEnTripulantes tr1 trs

hayTripulanteEnTripulantes :: Tripulante -> [Tripulante] -> Bool
hayTripulanteEnTripulantes tr1 [] = False
hayTripulanteEnTripulantes tr1 (tr : trs) = (tr1 == tr) || hayTripulanteEnTripulantes tr1 trs

-- Propósito: Devuelve los sectores en donde aparece un tripulante dado.

-- 3.7
tripulantes :: Nave -> [Tripulante]
tripulantes (N t) = tripulantesEnArbol t

tripulantesEnArbol :: Tree Sector -> [Tripulante]
tripulantesEnArbol EmptyT = []
tripulantesEnArbol (NodeT s t1 t2) = tripulantesEnSector s ++ tripulantesEnArbol t1 ++ tripulantesEnArbol t2

tripulantesEnSector :: Sector -> [Tripulante]
tripulantesEnSector (S _ _ trs) = sinRepetir trs

sinRepetir :: Eq a => [a] -> [a]
sinRepetir [] = []
sinRepetir (a:as) = if a == head as
                      then as
                      else (a:as)

--Propósito: Devuelve la lista de tripulantes, sin elementos repetidos

-- 4 MANADA DE LOBOS
type Presa = String -- nombre de presa
type Territorio = String -- nombre de territorio
type Nombre = String -- nombre de lobo
data Lobo = Cazador Nombre [Presa] Lobo Lobo Lobo | Explorador Nombre [Territorio] Lobo Lobo | Cría Nombre
data Manada = M Lobo

-- 4.1
manada1 = M (Cazador "c1" ["Conejo", "Ave"] 
              (Explorador "e1" ["Bosque Peras"]
                 ( Cría "cr1")
                  (Cría "cr2")
                )
              (Explorador "e2" ["Rio Anana", "Montaña Helado"]
                  (Cría "cr3")
                  (Cría "cr4")
                )
              (Cría "cr5")
            )

manada2 =
  M
    ( Cazador
        "c1"
        ["Conejo", "Ave", "Conejo2", "Liebre", "Jabalí", "Gato", "Venado"]
        ( Explorador
            "e1"
            ["Bosque Peras"]
            (Cría "cr1")
            (Cría "cr2")
        )
        ( Explorador
            "e2"
            ["Rio Anana", "Montaña Helado"]
            (Cría "cr3")
            (Cría "cr4")
        )
        (Cría "cr5")
    )

-- 4.2 
buenaCaza :: Manada -> Bool
buenaCaza (M l) = cantidadDeAlimentoCazado l > cantidadDeCrias l

cantidadDeAlimentoCazado :: Lobo -> Int
cantidadDeAlimentoCazado (Cría n) = 0
cantidadDeAlimentoCazado (Explorador n ts l1 l2) = (cantidadDeAlimentoCazado l1 + cantidadDeAlimentoCazado l2)
cantidadDeAlimentoCazado (Cazador n ps l1 l2 l3) = (length ps) + cantidadDeAlimentoCazado l1 + cantidadDeAlimentoCazado l2 + cantidadDeAlimentoCazado l3

esCría :: Lobo -> Bool
esCría (Cría _ ) = True
esCría _ = False

delta :: Bool -> Int
delta True = 1
delta False = 0

cantidadDeCrias :: Lobo -> Int
cantidadDeCrias (Cría n) = 0
cantidadDeCrias (Explorador n ts l1 l2) = (delta (esCría l1)) + (delta (esCría l2)) + cantidadDeCrias l1 + cantidadDeCrias l2
cantidadDeCrias (Cazador n ps l1 l2 l3) = (delta (esCría l1)) + (delta (esCría l2)) + (delta (esCría l3)) + cantidadDeCrias l1 + cantidadDeCrias l2 + cantidadDeCrias l3

-- Propósito: dada una manada, indica si la cantidad de alimento cazado es mayor a la cantidad de crías.


-- 4.3 
-- elAlfa :: Manada -> (Nombre, Int)
-- elAlfa (M l) = loboConMasPresas l

-- loboConMasPresas :: Lobo -> (Nombre, Int)
-- loboConMasPresas (Cría n) = (n, 0)
-- loboConMasPresas (Explorador n ts l1 l2) = if ( (not esCazador l1) || (not esCazador l2) )
--                                               then (n, 0)
--                                               elseif (cantidadDePresa l1 > cantidadDePresa l2)
--                                                   then (nombreLobo )
-- loboConMasPresas (Cazador n ps l1 l2 l3) = if ( ( ((length ps)> cantidadDePresa l1) || ((length ps)> cantidadDePresa l2) || ((length ps)>cantidadDePresa l3))) 
--                                               then 

-- nombreLobo :: Lobo -> Nombre -- String
-- nombreLobo (_ n _ ) = n

-- Propósito: dada una manada, devuelve el nombre del lobo con más presas cazadas, junto
-- con su cantidad de presas. Nota: se considera que los exploradores y crías tienen cero presas
-- cazadas, y que podrían formar parte del resultado si es que no existen cazadores con más de
-- cero presas.

-- 4. losQueExploraron :: Territorio -> Manada -> [Nombre]
-- Propósito: dado un territorio y una manada, devuelve los nombres de los exploradores que
-- pasaron por dicho territorio.