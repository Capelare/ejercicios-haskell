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

{-
Problema 3

Define una funci�n 'comprime' cuyo comportamiento sea el siguiente:

	comprime f g [x1..xn] = f x1(f x2(...(f xn-1 (g xn))))
	comprime f g [x] = g x
-}

comprime :: (a -> b -> b) -> (a -> b) -> [a] -> b
comprime f g [x] = g x
comprime f g (x:xs) = f x (comprime f g xs)

{-
Problema 4

Define una funci�n 'acumula' que reciba un dato y una lista ordenada de pares
donde la primera componente representa un dato y la segunda un contador y a�ada ese dato en la lista. Se considera que la lista est� ordenada por la primera componente.

-}

acumula :: Ord a => a -> [(a, Integer)] -> [(a, Integer)]
acumula x [] = [(x, 1)]
acumula x ((y,n):ys)
	| x > y = (y,n) : acumula x ys
	| x == y = (y, n + 1) : ys
	| otherwise = (x, 1) : ((y,n):ys)


{-
Problema 5

Define, usando 'foldr', una funci�n 'acumulado' que tome una lista y devuelva una lista ordenada de pares donde cada par representa el elemento y el n�mero de apariciones.

-}

acumulado :: Ord a => [a] -> [(a, Integer)]
acumulado = foldr acumula []

{-
Problema 6

Operaciones sobre �rboles binarios utilizando plegados.

-}

data ArbolB a = Vacio | NodoB (ArbolB a) a (ArbolB a) deriving Show

foldB :: (a -> b -> b -> b) -> b -> ArbolB a -> b
foldB f z Vacio = z
foldB f z (NodoB i r d) = f r (foldB f z i) (foldB f z d)

-- 'apariciones', dado un valor del tipo a y un �rbol, calcula el n�mero de vece-- que aparece el valor dado en el �rbol.

apariciones :: Eq a => a -> ArbolB a -> Int
apariciones e = foldB fb 0
	where
		fb r soli sold
			| e == r = 1 + soli + sold
			| otherwise = soli + sold
