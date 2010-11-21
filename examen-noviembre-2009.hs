{-
Problema 1

a) �Cu�l es el tipo polim�rfico de la funci�n siguiente?

	fun1 f x y = f x (f x y)

	fun1 :: (a -> b -> b) -> a -> b -> b

b) �Cu�l es el tipo polim�rfico de la funci�n siguiente?

	fun2 f x y = (f x, f x y)

	fun2 :: (a -> b -> c) -> a -> b -> (b -> c, c)

-}

{-
Problema 2

Dada la funci�n:
	
	f p g e [] = []
	f p g e (x:xs)
		| p (g x e) = (x, g x e) : f p g e xs
		| otherwise = f p g e xs

-}

f :: (c -> Bool) -> (a -> b -> c) -> b -> [a] -> [(a, c)]
f2 :: (c -> Bool) -> (a -> b -> c) -> b -> [a] -> [(a,c)]
f3 :: (c -> Bool) -> (a -> b -> c) -> b -> [a] -> [(a,c)]

-- a) Redefinici�n utilizando listas por comprensi�n

f p g e xs = [(x, y) | x <- xs, let y = g x e, p y] 

-- b) Refefinici�n utilizando 'filter' y 'map'

f2 p g e xs = map (\x -> (x, g x e)) (filter (\x -> p (g x e)) xs)

-- f2 = map (\x -> (x, g x e)) . filter (p . (flip g e))

-- c) Redefinici�n utilizando 'foldr'

f3 p g e = foldr fb []
	where
	 fb y ys
	 	| p (g y e) = (y, g y e) : ys
		| otherwise = ys
