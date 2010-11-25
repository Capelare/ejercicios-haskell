{-
Problema 1
¿Cuál es el tipo polimórfico de las funciones fun1 y fun2?

a) fun1 f x y = f x (f x y)
-}

-- fun1 :: (a -> b -> b) -> a -> b -> b

{-
b) fun2 f x y = (f x, f x y)
-}

-- fun2 :: (a -> b -> c) -> a -> b -> ((b -> c), c)

{-
Problema 2
Redefine la función:

	f p g e [] = []
	f p g e (x:xs)
	  | p (g x e) = (x, g x e) : f p g e xs
	  | otherwise = f p g e xs

usando:
a) listas por comprensión
-}

-- f p g e xs = [(x, g x e) | x <- xs, p (g x e)]

{-
b) Filter y Map
-}

-- f p g e = map (\x -> (x, g x e)) . filter (p . (flip g e))

{-
c) foldr
-}

--f p g e = foldr fb []
--  where
--    fb x s
--      | p (g x e) = (x, g x e):s
--      | otherwise = s

{-
Problema 3
Define una función comprim, dando también su tipo, cuyo comportamiento sea el siguiente:
	comprime f g [x1,x2,...,xn-1,xn] => f x1 (f x2 (...(f xn-1 (g xn))...))
	comprime f g [x] => g x
	
Por ejemplo:
	comprime (+) (*2)  [1,2,3,4] ==> 14
	comprime max id [1,-4,7,3,2] ==> 7

-}

comprime :: (a -> b -> b) -> (a -> b) -> [a] -> b
comprime f g [x] = g x
comprime f g (x:xs) = f x (comprime f g xs)

{-
Problema 4
Define la función acumula
	acumula :: Ord a => a -> [(a,Integer)] -> [(a, Integer)]
que reciba un dato y una lista ordenada de pares donde la primera componente representa un dato
y la segunta un contador y añada este dato en la lista. Se considera que la lista está ordenada
por la primera componente. Por ejemplo:
	acumula 'c' [('a', 2), ('c', 3), ('f', 5), ('g', 1)] ==>
		[('a', 2), ('c', 4), ('f', 5), ('g', 1)]
	acumula ’c’ [(’a’, 2), (’b’, 3), (’f’, 5), (’g’, 1)] ==>
		[(’a’, 2), (’b’, 3), ('c', 1), (’f’, 5), (’g’, 1)]
-}

acumula x [] = [(x,1)]
acumula x ((y,n):ys)
  | x < y = (x,1): (y,n) : ys
  | x == y = (y,(n+1)) : ys
  | otherwise = (y,n) : (acumula x ys)

{-
Problema 5
Define, usando foldr, una función acumulado que tome una lista y devuelva una lista ordenada
de pares donde cada par representa el elemento y el número de apariciones. Indica también
su tipo. Ejemplo:

	acumulado "abracadabra" ==> [('a',5),('b',2),('c',1),('d',1),('r',2)]
	acumulado  [3,1,2,1,8,8,2,8,8,2,8] ==> [(1,2),(2,3),(3,1),(8,5)]
-}

acumulado :: Ord a => [a] -> [(a,Integer)]
acumulado = foldr fb []
  where
	fb x sol = acumula x sol

{-
Problema 6
Dada la siguiente definicion de árbol binario:
-}
data ArbolB a = Vacio | NodoB a (ArbolB a) (ArbolB a) deriving Show
{-
y la función de plegado:
-}
foldB :: (a -> b -> b -> b) -> b -> ArbolB a -> b
foldB f z Vacio = z
foldB f z (NodoB r iz de) = f r (foldB f z iz) (foldB f z de)
{-
que se puede definir alternativamente como:
	foldB f base = resolver
	  where
		resolver Vacio = base
		resolver (NodoB r iz de) = f r (resolver iz) (resolver de)

Define la función

	apariciones :: Eq a => a -> ArbolB a -> Int

que, dados un valor del tipo a y un árbol, calcule el número de veces que aparece el valor dado en el
árbol. Por ejemplo:

	apariciones 3 Vacio ==> 0
	apariciones 3 (NodoB 5 (NodoB 3 Vacio Vacio) (NodoB 4 (NodoB 3 Vacio Vacio) Vacio)) ==> 2
	
a) mediante recursión:
-}

apariciones :: Eq a => a -> ArbolB a -> Int
apariciones _ Vacio = 0
apariciones x (NodoB r iz de)
  | x == r = 1 + (apariciones x iz) + (apariciones x de)
  | otherwise = (apariciones x iz) + (apariciones x de)

{-
b) mediante foldB
-}

aparicionesf :: Eq a => a -> ArbolB a -> Int
aparicionesf x = foldB f 0
  where
	f r soli sold
	  | r == x = 1 + soli + sold
	  | otherwise = soli + sold



















