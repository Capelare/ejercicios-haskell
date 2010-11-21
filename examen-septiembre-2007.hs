{-
Ejercicio 1

a) ¿Cuál es el tipo polimórfico de 'fun'?

	fun f g x = f g x

	fun :: (a -> b -> c) -> a -> b -> c

b) ¿Cuál es el tipo polimórfico de 'fun2'?

	fun2 f g x = f . (g x)

-- TODO

c) ¿Cuál es el tipo polimórfico de 'fun3'?

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

-- definir una función 'mkAlbaran' que, dada una compra en la que pueden 
-- aparecer códigos de productos repetidos en cualquier orden, y una base
-- de datos con las características de cada producto produzca el correspondiente-- albarán.

mkAlbaran :: BD -> Compra -> Albaran
-- mkAlbaran bd cs = mkAlb bd (agrupar cs)

mkAlb bd [] = []
mkAlb bd (c,n):cns) = (n, nom, pr) : mkAlb bd cns
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

{-
Ejercicio 3

-}

-- TODO
