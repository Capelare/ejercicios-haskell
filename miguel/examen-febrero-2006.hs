{-
Ejercicio 1

a) Reescribe la función 'g' con una lista por comprensión.

	g x = map (aplicaA x)
	aplicaA x f = f x	
-}

-- aplicaA :: a -> (a -> b) -> b


g :: a -> [a -> b] -> [b]
g x fs = [f x | f <- fs]

{-
b) Da el tipo polimórfico de la función delete:
	
	delete x [] = []
	delete x (y:ys) = if x == y then ys else y : delete x ys
-}

-- delete :: Eq a => a -> [a] -> [a]

{-
c) Da el tipo polimórfico de la función flip:

	flip f x y = f y x

-}

-- flip :: (a -> b -> c) -> b -> a -> c

{-
d) Da el tipo polimórfico del operador (\\):
	
	(\\) = foldl (flip delete)
-}

-- (\\) :: Eq a => [a] -> [a] -> [a]

{-
e) Da el tipo polimórfico de la función f. Explica además qué hace:

	f a = (a\\) . (a\\)
-}

-- f :: Eq a => [a] -> [a] -> [a]

-- f xs ys = xs - (xs - ys). 	Es decir, se eliminan de xs los elementos que NO estén en ys,
--								o lo que es lo mismo, se devuelven los elementos de ys que estén en xs

{-
f) Considera las siguientes definiciones del Prelude:

	data Maybe a = Nothing | Just a deriving (Eq, Ord, Read, Show)
	
	lookup key [] = Nothing
	lookup key ((x,y):xys)
	  | key == x = Just y
	  | otherwise = lookup key xys
	
	define f y z para que la siguiente definición sea equivalente a la anterior:
	
		lookup key = foldr f z
		  where
			...
-}

lookup key = foldr f z
  where
	z = Nothing
	f (x,y) sol
	  | key == x = Just y
	  | otherwise = sol

{-
g) Define f y z para que la siguiente definición sea equivalente a la anterior:
	lookup' key = foldl f z
	  where
		...
-}

lookup' key = foldl f z
  where
	z = Nothing
	f Nothing (x,y)
	  | key == x = Just y
	  | otherwise = Nothing
	f (Just y) (x,_) = Just y

{-
Ejercicio 2

Para representar un colgante de los que suele haber en las habitaciones de los niños definimos el siguiente tipo:
-}

type Masa = Int
type Longitud = Int
data Colgante = Figura Masa | Eje Rama Rama deriving Show
type Rama = (Longitud, Colgante)

{-
Por ejemplo, el colgante

     | 
 ===========			==== Ejes
 |         | 
========   40
|      |				| Cuerdas
30 =======
   |     |
   50    20

queda representado con la expresión

Eje (4, Eje (1, Figura 30) (7, Eje (5, Figura 50) (2, Figura 20))) (7, Figura 40)

a) Asumiendo que los ejes y las cuerdas del colgante no pesan, define una función
   masa :: Colgante -> Masa que calcule la masa total de un colgante.

-}

masa :: Colgante -> Masa
masa (Figura m) = m
masa (Eje (_,c1) (_,c2)) = (masa c1) + (masa c2)

{-
b) Define una función momento :: Rama -> Int que calcule el momento ejercido por
   una rama (la longitud de la rama por su masa)
-}

momento :: Rama -> Int
momento (l,c) = l * (masa c)

{-
c) Un colgante está balanceado si todos sus ejes están horizontales. Para que un eje esté horizontal
   los momentos de sus dos ramas deben coincidir. Define una función balanceado :: Colgante -> Bool
   que indique si un colgante está balanceado
-}

balanceado :: Colgante -> Bool
balanceado (Figura _) = True
balanceado (Eje r1@(_,c1) r2@(_,c2)) = (momento r1 == momento r2) && balanceado c1 && balanceado c2

-- ejemplo de arbol balanceado: Eje (4, Eje (7, Figura 10) (1, Eje (, Figura 20) (2, Figura 50))) (8, Figura 40)
