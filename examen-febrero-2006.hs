{-
Ejercicio 1

a) Reescribe la funci�n 'g' con una lista por comprensi�n.

	g x = map (aplicaA x)
	aplicaA x f = f x
	
	aplicaA :: a -> (a -> b) -> b
	
	g :: a -> [a -> b] -> [b]
	g x fs = [f x | f <- fs]
	
-- TODO
-}