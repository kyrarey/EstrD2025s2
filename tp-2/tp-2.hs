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
{-
aplanar :: [[a]] -> [a]
aplanar [] = []
aplanar (xs : xss) = xs : aplanar xss
-}

-- 7
pertenece :: (Eq a) => a -> [a] -> Bool
pertenece _ [] = False
pertenece e (x : xs) =
  if x == e
    then True
    else False || pertenece e xs

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
{-
reversa :: [a] -> [a]
reversa [] = []
reversa (x : xs) = reversa xs : x
-}

-- 14
zipMaximos :: [Int] -> [Int] -> [Int]
zipMaximos [] [] = []
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
factorial n = (n * antecesor n) + factorial (antecesor n)

antecesor :: Int -> Int
antecesor n = n - 1

-- 2
cuentaRegresiva :: Int -> [Int]
-- precondicion: el numero ingresado debe ser igual o mayor a 0
-- cuentaRegresiva MENSAJE DE ERROR CON NUMS MENORES A 0 ?
cuentaRegresiva 0 = []

cuentaRegresiva