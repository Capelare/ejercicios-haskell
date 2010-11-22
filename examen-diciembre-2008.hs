{-
Problema 1

a) Dar el tipo de la funci�n

	k f = (map f) . f
	
	k :: (a -> [a]) -> a -> [[a]]
	
b) Expresar la funci�n siguiente en t�rminos de 'map' y 
	'filter'
	
	k' p z f [] = []
	k' p z f (x:xs)
		| p z x = f x : k' p z f xs
		| otherwise = k' p z f xs
		
	k' p z f = map f . filter (p z)
	
c) �Qu� devuelve la expresi�n k' (<) 5 even [6, 2, 7, 3]?

	[True, False]
	
d) Expresar la funci�n k'' p f = (filter p) . (map f) como una 
	lista por comprensi�n
	
	k'' p f xs = [f x | x <- xs, p (f x)]
	
-}

{-
Problema 2
-}

-- Dada la siguiente definici�n de tipo

-- data Maybe a = Nothing | Just a deriving Show

{- y la siguiente funci�n que empareja los elementos de dos listas

zip :: [a] -> [b] -> [(a,b)]
zip(x:xs)(y:ys) = (x,y) : zip xs ys
zip _ _ = []
-}

-- a) Definir una funci�n 'primeraPosicion' que devuelva la primera
--		posici�n que ocupa un elemento en la lista. Nothing si no est�.

primeraPosicion :: Eq a => a -> [a] -> Maybe Int
primeraPosicion x ls = pos x (zip ls [0..])
	where
		pos _ [] = Nothing
		pos x ((y,n):ys)
			| x == y = Just n
			| otherwise = pos x ys
			
-- b) Expresar la funci�n usando 'foldl'
{-
foldl f z []     = z       
foldl f z (x:xs) = foldl f (f z x) xs 
-}

prPosicion :: Eq a => a -> [a] -> Maybe Int
prPosicion x ls = foldl fb Nothing (zip ls [0..])
	where
		fb (Just q) _ = Just q
		fb Nothing (y,n)
			| x == y = Just n
			| otherwise = Nothing
			
-- c) Definir la funci�n 'posiciones' que, dado un elemento y una lista,
--		devuelva una lista con todas las posiciones que ocupa dicho elemento
--		en la lista.

posiciones :: Eq a => a -> [a] -> [Int]
posiciones x ls = foldl fb [] (zip ls [0..])
	where
		fb ys (y,n)
			| x == y = ys ++ [n]
			| otherwise = ys
			
-- d) Expresar la funci�n anterior usando una lista por compresi�n y la
--		funci�n 'zip'

posiciones2 :: Eq a => a -> [a] -> [Int]
posiciones2 x ls = [n | (y, n) <- (zip ls [0..]), x == y]

{-
Problema 3
-}

-- Dado el siguiente tipo para representar �rboles no vac�os

data Arbol23 a = Hoja a
				| Nodo2 a (Arbol23 a) (Arbol23 a)
				| Nodo3 a (Arbol23 a) (Arbol23 a) (Arbol23 a) deriving Show
				
-- a) Definir la funci�n 'espejo', que devuelve la imagen especular
--		de un �rbol dado.

espejo :: Arbol23 a -> Arbol23 a
espejo (Hoja x) = Hoja x
espejo (Nodo2 x i d) = Nodo2 x (espejo d) (espejo i)
espejo (Nodo3 x i c d) = Nodo3 x (espejo d) (espejo c) (espejo i)

-- b) Definir la funci�n 'pertenece', que comprueba si un elemento
--		pertenece al �rbol.

pertenece :: Eq a => a -> Arbol23 a -> Bool
pertenece e (Hoja x) = e == x
pertenece e (Nodo2 x i d) = (e == x) || (pertenece e i) || (pertenece e d)
pertenece e (Nodo3 x i c d) = (e == x) || (pertenece e i) || (pertenece e c) || (pertenece e d)

-- c) Definir la funci�n de plegado 'foldA23' para este tipo de �rboles.

foldA23 :: (a -> b -> b -> b -> b) -> (a -> b -> b -> b) -> (a -> b) -> Arbol23 a -> b
foldA23 f g h = fun
	where
		fun (Hoja x) = h x
		fun (Nodo2 x i d) = g x (fun i) (fun d)
		fun (Nodo3 x i c d) = f x (fun i) (fun c) (fun d)
		
-- d) Expresar las funciones 'espejo' y 'pertenece' en funci�n de 'foldA23'.

espejofoldA23 :: Arbol23 a -> Arbol23 a
espejofoldA23 = foldA23 (\r i c d -> Nodo3 r d c i) (\r i d -> Nodo2 r d i) Hoja

pertenecefoldA23 :: Eq a => a -> Arbol23 a -> Bool
pertenecefoldA23 x = foldA23 (\r i c d -> (x == r) || i || c || d) (\r i d -> (x == r) || i || d) (x ==)