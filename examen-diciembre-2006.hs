{-
Ejercicio 1


-- a) Da el tipo polimórfico de la función

	k f p x xs = [f y x | y <- xs, p x y]


	k :: (a -> b -> c) -> (b -> a -> Bool) -> b -> [a] -> [c]

-- b) Escribe la función anterior usando las funciones map y filter

	k f p x xs = map (\y -> f y x) (filter (p x) xs)

-- c) Escribe la función anterior usando foldr

	k f p x = foldr fun
		where
			fun [] = []
			fun (y:ys) = if p x y then (f y x) : fun xs else fun xs 

-- d) Da un ejemplo de uso de la función anterior e indica el resultado

	k suma (<) 3 [3,5,6,1]

	[Integer] : [8, 9]

-}

{-
Ejercicio 2
-}

-- a) Dada una lista, devuelve un par formado por el mayor prefijo ordenado
--	ascendentemente y el resto de la lista

ultimoOrdenado :: Ord a => [a] -> Int
ultimoOrdenado [] = 0
ultimoOrdenado [x] = 1
ultimoOrdenado (x:y:ys)
	| x <= y = 1 + ultimoOrdenado (y:ys)
	| otherwise = 1

mayorPrefijo :: Ord a => [a] -> ([a],[a])
mayorPrefijo xs = splitAt (ultimoOrdenado xs) xs

-- b) Dada una lista, devuelve el segmento más largo ordenado ascendentemente 
--	que aparezca en dicha lista.

segmentosOrdenados :: Ord a => [a] -> [[a]]
segmentosOrdenados [] = [[]]
segmentosOrdenados (x:xs) = (\(x,y) -> x) (mayorPrefijo (x:xs)) : segmentosOrdenados xs

masLargo :: [[a]] -> [a]
masLargo [] = []
masLargo [x] = x
masLargo (x:y:ys)
	| (length x) > (length y) = masLargo (x:ys)
	| otherwise = masLargo (y:ys)

mayorSegmento :: Ord a => [a] -> [a]
mayorSegmento xs = masLargo (segmentosOrdenados xs)

{- c) Sea la siguiente función 'scanl' predefinida en Prelude de Haskell:

	scanl :: (a -> b -> a) -> a -> [b] -> a
	scanl f y [] = [y]
	scanl f y (x:xs) = y : scanl f (f y x) xs

	define a partir de 'scanl' una función no recursiva 'facts' que dado
	un número n, devuelva una lista con los factoriales desde 0! hasta n!
-}

factorial :: Integer -> Integer
factorial 0 = 1
factorial n@(m + 1) = n * factorial m

facts :: Integer -> [Integer]
facts 0 = [1]
facts n = scanl (\y x -> factorial x)  (factorial 0) [1..n]