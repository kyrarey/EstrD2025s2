-- PRACTICA 3

-- 1 TIPOS RECURSIVOS SIMPLES

-- 1.1 CELDAS CON BOLITAS
data Color = Azul | Rojo

data Celda = Bolita Color Celda | CeldaVacia

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
ponerN 0 c cel = cel
ponerN n c (Bolita c2 cel) =
  if n /= 1
    then ponerN (n - 1) c (poner c cel)
    else poner c cel