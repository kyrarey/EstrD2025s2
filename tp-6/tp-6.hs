import PriorityQueue

-- PRACTICA 6


--1. Priority Queue (cola de prioridad)

pq1 = [1, 4, 7, 9, 23, 65, 56, 79]

--Ejercicio 2
-- Implementar la función heapSort :: Ord a => [a] -> [a], que dada una lista la ordena de
-- menor a mayor utilizando una Priority Queue como estructura auxiliar. Cuál es su costo?
-- OBSERVACIÓN: el nombre heapSort se debe a una implementación particular de las Priority
-- Queues basada en una estructura concreta llamada Heap, que será trabajada en la siguiente
-- práctica.

listToPQ :: Ord a => [a] -> PriorityQueue a
listToPQ [] = emptyPQ
listToPQ (x:xs) = insertPQ x (listToPQ xs )

-- heapSort :: Ord a => [a] -> [a]
-- heapSort xs = subtarea1 (subtarea2 xs)