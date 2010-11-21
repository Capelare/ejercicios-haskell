{-
Problema 1

a) ¿Cuál es el tipo polimórfico de la función siguiente?

	fun1 f x y = f x (f x y)

	fun1 :: (a -> b -> b) -> a -> b -> b

b) ¿Cuál es el tipo polimórfico de la función siguiente?

	fun2 f x y = (f x, f x y)

	fun2 :: (a -> b -> c) -> a -> b -> (b -> c, c)

-}

{-
Problema 2

Dada la función:
	
	f p g e [] = []
	f p g e (x:xs)
		| p (g x e) = (x, g x e) : f p g e xs
		| otherwise = f p g e xs

-}

f :: (c -> Bool) -> (a -> b -> c) -> b -> [a] -> [(a, c)]
f2 :: (c -> Bool) -> (a -> b -> c) -> b -> [a] -> [(a,c)]
f3 :: (c -> Bool) -> (a -> b -> c) -> b -> [a] -> [(a,c)]

-- a) Redefinición utilizando listas por comprensión

f p g e xs = [(x, y) | x <- xs, let y = g x e, p y] 

-- b) Refefinición utilizando 'filter' y 'map'

f2 p g e xs = map (\x -> (x, g x e)) (filter (\x -> p (g x e)) xs)

-- f2 = map (\x -> (x, g x e)) . filter (p . (flip g e))

-- c) Redefinición utilizando 'foldr'

f3 p g e = foldr fb []
	where
	 fb y ys
	 	| p (g y e) = (y, g y e) : ys
		| otherwise = ys
