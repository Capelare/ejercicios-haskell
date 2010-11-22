{-
Ejercicio 1

a) ?Cu?l es el tipo polim?rfico de 'fun'?

	fun f g x = f g x

	fun :: (a -> b -> c) -> a -> b -> c

b) ?Cu?l es el tipo polim?rfico de 'fun2'?

	fun2 f g x = f . (g x)

-- TODO

c) ?Cu?l es el tipo polim?rfico de 'fun3'?

	fun3 f g x = (f g) . x

	fun3 :: (a -> b -> c) -> a -> (d -> b) -> d -> c

-}

{-
Ejercicio 2
-}

-- Dadas las siguientes definiciones de tipos

type Codigo = Integer
type Unidades = Integer
type Nombre = String
type PrecioUnitario = Double
type Producto = (Codigo, Nombre, PrecioUnitario)
type BD = [Producto]
type Compra = [Codigo]
type Albaran = [(Unidades, Nombre, PrecioUnitario)]

-- definir una funci?n 'mkAlbaran' que, dada una compra en la que pueden 
-- aparecer c?digos de productos repetidos en cualquier orden, y una base
-- de datos con las caracter?sticas de cada producto produzca el correspondiente-- albar?n.
{-
mkAlbaran :: BD -> Compra -> Albaran
mkAlbaran bd cs = mkAlb bd (agrupar cs)

mkAlb bd [] = []
mkAlb bd ((c,n):cns) = (n, nom, pr) : mkAlb bd cns
	where
		(_, nom, pr) = producto c bd


producto :: Codigo -> BD -> Producto
producto x [] = error "producto desconocido"
producto x ((c, n, p):ls) = if x == c then (c, n, p) else producto x ls

-- expresar 'mkAlbaran' con ayuda de foldr

mkAlbaran bd cs = foldr f [] (agrupar cs)
		where 
			f (c, n) rs = (n, nom, pr) : rs
				where (_, nom, pr) = producto c bd
-}
{-
Ejercicio 3

-}

-- Dada la siguiente definición de tipo

data Problema a = Cal a | Sub [Problema a] deriving Show

-- utilizado para representar los puntos correspondientes a los distintos
-- apartados de un problema de examen.

-- a) Define la función de plegado para el tipo Problema a

foldProblema :: ([b] -> b) -> (a -> b) -> Problema a -> b
foldProblema f g (Cal x) = g x
foldProblema f g (Sub xs) = f (map (foldProblema f g) xs)

-- b) Usando la función 'foldProblema', define una función 'notaTotal' que
--		calcule la puntuación total de un examen.

notaTotal :: Num a => Problema a -> a
notaTotal = foldProblema sum (id)

-- c) Usando la función 'foldProblema', define una función 'maximaNota' que
--		devuelva la nota con mayor valoración de un exámen.

maximaNota :: (Num a, Ord a) => Problema a -> a
maximaNota = foldProblema maximum (id)

