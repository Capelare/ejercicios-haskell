{-
Ejercicio 1

a) Reescribe la funci�n 'g' con una lista por comprensi�n.

	g x = map (aplicaA x)
	aplicaA x f = f x	
-}

-- aplicaA :: a -> (a -> b) -> b


g :: a -> [a -> b] -> [b]
g x fs = [f x | f <- fs]

{-
b) Da el tipo polim�rfico de la funci�n delete:
	
	delete x [] = []
	delete x (y:ys) = if x == y then ys else y : delete x ys
-}

-- delete :: Eq a => a -> [a] -> [a]

{-
c) Da el tipo polim�rfico de la funci�n flip:

	flip f x y = f y x

-}

-- flip :: (a -> b -> c) -> b -> a -> c

{-
d) Da el tipo polim�rfico del operador (\\):
	
	(\\) = foldl (flip delete)
-}

-- (\\) :: Eq a => [a] -> [a] -> [a]

{-
e) Da el tipo polim�rfico de la funci�n f. Explica adem�s qu� hace:

	f a = (a\\) . (a\\)
-}

-- f :: Eq a => [a] -> [a] -> [a]

-- f xs ys = xs - (xs - ys). 	Es decir, se eliminan de xs los elementos que NO est�n en ys,
--								o lo que es lo mismo, se devuelven los elementos de ys que est�n en xs

{-
f) Considera las siguientes definiciones del Prelude:

	data Maybe a = Nothing | Just a deriving (Eq, Ord, Read, Show)
	
	lookup key [] = Nothing
	lookup key ((x,y):xys)
	  | key == x = Just y
	  | otherwise = lookup key xys
	
	define f y z para que la siguiente definici�n sea equivalente a la anterior:
	
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
g) Define f y z para que la siguiente definici�n sea equivalente a la anterior:
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

Para representar un colgante de los que suele haber en las habitaciones de los ni�os definimos el siguiente tipo:
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

queda representado con la expresi�n

Eje (4, Eje (1, Figura 30) (7, Eje (5, Figura 50) (2, Figura 20))) (7, Figura 40)

a) Asumiendo que los ejes y las cuerdas del colgante no pesan, define una funci�n
   masa :: Colgante -> Masa que calcule la masa total de un colgante.

-}

masa :: Colgante -> Masa
masa (Figura m) = m
masa (Eje (_,c1) (_,c2)) = (masa c1) + (masa c2)

{-
b) Define una funci�n momento :: Rama -> Int que calcule el momento ejercido por
   una rama (la longitud de la rama por su masa)
-}

momento :: Rama -> Int
momento (l,c) = l * (masa c)

{-
c) Un colgante est� balanceado si todos sus ejes est�n horizontales. Para que un eje est� horizontal
   los momentos de sus dos ramas deben coincidir. Define una funci�n balanceado :: Colgante -> Bool
   que indique si un colgante est� balanceado
-}

balanceado :: Colgante -> Bool
balanceado (Figura _) = True
balanceado (Eje r1@(_,c1) r2@(_,c2)) = (momento r1 == momento r2) && balanceado c1 && balanceado c2

-- ejemplo de arbol balanceado: Eje (4, Eje (7, Figura 10) (1, Eje (, Figura 20) (2, Figura 50))) (8, Figura 40)
