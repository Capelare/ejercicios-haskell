{-
Problema 1
¿Cuáles son los tipos polimórficos de las siguientes funciones?

a) fun1 f g = map (f . g)
-}

fun1 :: (b -> c) -> (a -> b) -> [a] -> [c]

{-
b) fun2 f g = (map f) . g
-}

fun2 :: (b -> c) -> (a -> [b]) -> a -> [c]

{-
c) fun3 f p q xs = [p (f x) | x <- xs, q x]
-}

fun3 :: (a -> b) -> (b -> c) -> (a -> Bool) -> [a] -> [c]

{-
Problema 2
Define una función cortesPropios que dada una lista xs devuelva todos los pares (ys,zs)
de listas no vacías tales que ys++zs == xs. Por ejemplo:

	cortesPropios [1..4] ==> [([1],[2,3,4]),([1,2],[3,4]),([1,2,3],[4])]
-}

cortesPropios :: [a] -> [([a],[a])]
cortesPropios [] = []
cortesPropios [x] = []
cortesPropios 