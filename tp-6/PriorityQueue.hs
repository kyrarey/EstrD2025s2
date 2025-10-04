module PriorityQueue 
    (PriorityQueue, emptyPQ, isEmptyPQ, insertPQ, findMinPQ, deleteMinPQ)
where

data PriorityQueue a = PQ [a]
{- INV. REP.: en PQ xs:
            * los elementos en xs estan ordenados segun su prioridad, siendo el primero el
            mas prioritario. (implementacion con lista ordenada)
-}

--Ejercicio 1
--Implementarla usando listas, e indicando el costo de cada operación.
emptyPQ :: PriorityQueue a
--Propósito: devuelve una priority queue vacía.
isEmptyPQ :: PriorityQueue a -> Bool
--Propósito: indica si la priority queue está vacía.
insertPQ :: Ord a => a -> PriorityQueue a -> PriorityQueue a
--Propósito: inserta un elemento en la priority queue.
findMinPQ :: Ord a => PriorityQueue a -> a
--Propósito: devuelve el elemento más prioriotario (el mínimo) de la priority queue.
--Precondición: parcial en caso de priority queue vacía.
deleteMinPQ :: Ord a => PriorityQueue a -> PriorityQueue a
--Propósito: devuelve una priority queue sin el elemento más prioritario (el mínimo).
--Precondición: parcial en caso de priority queue vacía.

emptyPQ = PQ []

--isEmptyPQ (PQ []) = True (no es necesario porque null cubre el caso base)
isEmptyPQ (PQ xs) = null xs

insertPQ x (PQ xs) = PQ (x:xs)

findMinPQ (PQ xs) = minimum xs --da el minimo de toda lista que sea ordenable

deleteMinPQ (PQ xs) = PQ (eliminaMin xs)

eliminaMin :: Ord a => [a] -> [a]
eliminaMin (x:xs) = if x < (minimum xs )
                        then xs
                        else eliminaMin xs

