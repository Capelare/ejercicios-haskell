{-
Ejercicio 3.2
Dada una cantidad de segundos, devuelve la cantidad de horas, minutos y segundos
equivalente.
-}

segundosAHora :: Integer -> (Integer,Integer,Integer)
segundosAHora s = (horas, minutos, segundos)
	where 
		horas = div s 3600
		ss = mod s 3600
		minutos = div ss 60
		segundos = mod ss 60


{- 
Ejercicio 3.3 
Incrementa todos los elementos de una tupla de tres enteros.
-}

incTupla3 :: (Integer, Integer, Integer) -> (Integer, Integer, Integer)
incTupla3 (x, y, z) = (x + 1, y + 1, z + 1)

{- 
Ejercicio 3.4
Incrementa todos los elementos de una lista de enteros.
-}

incLista :: [Integer] -> [Integer]
incLista [] = []
incLista (x:xs) = (x + 1) : incLista xs

{-
Ejercicio 3.5
Definición de un operador que aplica una lista de funciones a un entero
y devuelve la lista de enteros de los resultados.
-}

(|>) :: [(Integer ->Integer)] -> Integer -> [Integer]
(|>) ls x = case ls of
		[] -> []
		f:fs -> (f x) : (|>)fs x

{-
Ejercicio 3.6
Determina si un año es bisiesto.
-}

esBisiesto :: Integer -> Bool
esBisiesto x 
    |(mod x 4) == 0 = if ((mod x 100) == 0) && ((mod x 400) == 0) then False else True
    |otherwise = False


{-
Ejercicio 3.7
Resto de forma recursiva utilizando sustracciones.
-}

resto :: Integer -> Integer -> Integer
resto x y
    |x < y = x
    |otherwise = resto (x - y) y

{-
Ejercicio 3.8
Cociente de forma recursiva utilizando sumas y restas.
-}

cociente :: Integer -> Integer -> Integer
cociente x y
    |x < y = 0
    |otherwise = 1 + cociente (x - y) y

{-
Ejercicio 3.9
Función recursiva que devuelve el sumatorio desde un valor entero hasta otro.
-}

sumatorio :: Integer -> Integer -> Integer
sumatorio a b
    |a < b = a + sumatorio (a + 1) b
    |a == b = b
    |otherwise = error "el primer argumento es mayor que el segundo"

{-
Ejercicio 3.10
Función recursiva que calcula el producto de los números que hay entre el
primer y segundo argumento, ambos incluidos.
-}

productoIntervalo :: Integer -> Integer -> Integer
productoIntervalo a b
    |a < b = a * productoIntervalo (a + 1) b
    |a == b = b
    |otherwise = error "el primer argumento es mayor que el segundo"

{-
Ejercicio 3.11
Función que calcula el número de variaciones de n elementos tomados de
m en m.
-}

factorial :: Integer -> Integer
factorial 0 = 1
factorial m@(n + 1) = m * (factorial n)

variaciones :: Integer -> Integer -> Integer
variaciones n m = if n > m then error "n no puede ser mayor que m" else div (factorial m) (factorial (m - n))

variaciones2 :: Integer -> Integer -> Integer
variaciones2 n m = if n > m then error "n no puede ser mayor que m" else productoIntervalo (m - n + 1) m

{-
Ejercicio 3.12
Función que calculan números combinatorios.
-}

-- ( m n ) = m! / (m - n)! * n!
combinatorios :: Integer -> Integer -> Integer
combinatorios _ 0 = 1
combinatorios m n 
	| m == n = 1
	| otherwise = div (factorial m) ((factorial (m - n))*(factorial n))

{-
Ejercicio 3.13
N-ésimo término de la sucesión de Fibonacci.
-}

-- Es ineficiente
fibonacci :: Integer -> Integer
fibonacci 0 = 0
fibonacci 1 = 1
fibonacci(m + 2) = fibonacci m + fibonacci (m + 1)

{-
Ejercicio 3.14
Funciones que devuelven el mayor de dos, tres y cuatro números enteros.
-}

mayor :: Integer -> Integer -> Integer
mayor x y = div ((x + y) + abs(x - y)) 2

mayor3 :: Integer -> Integer -> Integer -> Integer
mayor3 x y z = mayor x (mayor y z)

mayor4 :: Integer -> Integer -> Integer -> Integer -> Integer
mayor4 w x y z = mayor (mayor w x) (mayor y z)

{-
Ejercicio 3.15
Función que toma tres números enteros y devuelve una terna con los números
en orden creciente.
-}

-- TODO

{-
Ejercicio 3.16
Función que determina si un número positivo de cuatro cifras es capicúa
-}

-- Sean las cifras abcd, compara (a == d) && (b == c)

esCapicua :: Integer -> Bool
esCapicua x 
    |x > 9999 = error "argumento mayor que 9999"
    |x > 999 = ((div x 1000) == (mod x 10)) && 
		((mod (div x 100) 10) == (div (mod x 100) 10))
    |otherwise = error "argumento menor que 999"


{-
Ejercicio 3.17
Función que calcula la suma de las cifras de un número natural.
-}

sumaCifras :: Integer -> Integer
sumaCifras x
    |x < 10 = x
    |otherwise = (mod x 10) + sumaCifras (div x 10)

{-
Ejercicio 3.18
Función que calcula el número de cifras de un número natural.
-}

numeroCifras :: Integer -> Integer
numeroCifras x
    |x < 10 = 1
    |otherwise = 1 + numeroCifras(div x 10)

{-
Ejercicio 3.19
Funciones que transforman una lista de dígitos en su correspondiente valor
entero y viceversa.
-}

aEntero :: [Integer] -> Integer
aEntero ls = case ls of
		[] -> 0
		x:xs -> x*10^(length xs) + aEntero xs

aLista :: Integer -> [Integer]
aLista x
    |x < 10 = [x]
    |otherwise = aLista (div x 10) ++ [(mod x 10)]

{-
Ejercicio 4.2
Función que calcula la derivada de una función de reales en reales en un
punto.
-}

-- TODO: No sé si está bien

derivada :: (Float -> Float) -> Float -> Float
derivada f x = (f (x + 0.00000001) - f x) / 0.00000001

coseno :: Float -> Float
coseno = derivada sin

{-
Ejercicio 4.3
FUnción que calcula el logaritmo de un número en una base dada, y en base dos.
-}

logEnBase :: Float -> Float -> Float
logEnBase b x = (log x) / (log b)

logEnBase2 :: Float -> Float
logEnBase2 = logEnBase 2

{-
Ejercicio 5.4
Operador que aplica una composición de funciones a un argumento.
-}

infixr 0 >$>
(>$>) :: (a -> a) -> a -> a
(>$>) = \f x -> f (f x)


{-
Ejercicio 5.5
Operador que aplica una composición de funciones a un argumento.
-}

infixr 9 >.>
f >.> g = \x -> g (f x)

{-
Ejercicio 5.6
Algunas funciones definidas usando la función polimórfica 'iter'.
-}

-- aplica la función que se le pasa como argumento
-- tantas veces como se le especifica
iter :: (Integer -> a -> a) -> a -> Integer -> a
iter op e 0 = e
iter op e m@(n + 1) = op m (iter op e n)

-- devuelve una lista decreciente desde el entero que se le pasa como argumento
listaDecre :: Integer -> [Integer]
listaDecre x = iter (:) [] x

-- devuelve un string con tantos palos como el valor del argumento

insertaPalo :: Integer -> String -> String
insertaPalo x c = "|" ++ c

palos :: Integer -> String
palos x = iter (insertaPalo) "" x

{-
Ejercicio 5.7
Función para invertir una lista.
-}

invierte :: [a] -> [a]
invierte ls = case ls of
		[] -> []
		[x] -> [x]
		x:xs -> invierte xs ++ [x]

{-
Ejercicio 5.8
-}

twice f x = f (f x)

{-
Ejercicio 5.9
Función equivalente a 'zip', empareja dos listas.
-}

emparejaListas :: [a] -> [a] -> [(a,a)]
emparejaListas [] _ = []
emparejaListas _ [] = []
emparejaListas (x:xs) (y:ys) = (x,y) : (emparejaListas xs ys)

{-
Ejercicio 6.1
Tipo de datos que representa temperaturas en grados centígrados y fahrenheit.
Función que comprueba si el agua está congelada a esa temperatura.
-}

data Temp = Centigrado Float | Farenheit Float deriving Show

estaCongelada :: Temp -> Bool
estaCongelada (Centigrado grados) = (grados <=0.0)
estaCongelada (Farenheit grados) = (grados <= 32)

{-
Ejercicio 6.2
Definición del tipo de datos Figura y función que calcula el perímetro de una
figura.
-}

type Radio = Float
type Lado = Float

data Figura = Circulo Radio
			| Cuadrado Lado
			| Rectangulo Lado Lado
			| Punto
			  deriving Show
			
perimetro :: Figura -> Float
perimetro (Circulo radio) = 2 * pi * radio
perimetro (Cuadrado lado) = 4 * lado
perimetro (Rectangulo ancho alto) = 2 * ancho + 2 * alto
perimetro (Punto) = error "no se puede calcular el perimetro del punto"

{-
Ejercicio 6.3
Definición de tipos Real, Imaginario, Complejo y Resultado. El tipo Resultado 
almacena las soluciones de una ecuacion de segundo grado.
La función 'raices' calcula las raíces de una ecuación de segundo grado de
la forma: ax^2 + bx + c = 0
-}

{- 
TODO : números real e imaginario...

type Real = Float
type Imag = Float
-}

data Complejo = Float :- Float deriving Show

data Resultado = UnaReal Float
			   | DosReales Float Float
			   | DosComplejas Complejo Complejo
				 deriving Show
				 

{-
TODO : raices..

raices :: Float -> Float -> Float -> Resultado
raices 
-}

{-
Ejercicio 6.4
Dado el tipo recursivo Nat, algunas operaciones sobre
dicho tipo.
-}

data Nat = Cero | Suc Nat deriving Show

cinco :: Nat
cinco = Suc(Suc(Suc(Suc(Suc Cero))))

dos :: Nat
dos = Suc(Suc Cero)

uno :: Nat
uno = Suc Cero

-- resta
resta :: Nat -> Nat -> Nat
resta Cero _ = Cero
resta x Cero = x
resta (Suc x) (Suc y) = resta x y

{- Funciones de plegado, más abajo
-- suma
suma :: Nat -> Nat -> Nat
suma x Cero = x
suma x (Suc y) = suma (Suc x) y

-- producto
producto :: Nat -> Nat -> Nat
producto _ Cero = Cero
producto x (Suc y) = suma x (producto x y)
-}

-- potencia
potencia :: Nat -> Nat -> Nat
potencia _ Cero = Suc Cero
potencia Cero _ = Cero
potencia x (Suc Cero) = x
potencia x (Suc y) = producto x (potencia x y)

-- convierte un número de Integer a Nat

intANat :: Integer -> Nat
intANat 0 = Cero
intANat (n + 1) = Suc(intANat(n))

-- convierte de Nat a Integer

natAInt :: Nat -> Integer
natAInt Cero = 0
natAInt (Suc x) = 1 + natAInt x

-- cociente y resto de dividir dos Nat

cocNat :: Nat -> Nat -> Nat
cocNat x y = intANat(cociente (natAInt x) (natAInt y))

resNat :: Nat -> Nat -> Nat
resNat x y = intANat(resto (natAInt x) (natAInt y))

{-
Ejercicio 6.5
Definir las funciones suma y productoS con la función de plegado
foldNat.
-}

foldNat :: (a -> a) -> a -> (Nat -> a)
foldNat f e Cero = e
foldNat f e (Suc n) = f(foldNat f e n)

{-
suma :: Nat -> Nat -> Nat
suma x Cero = x
suma x (Suc y) = Suc(suma x y)
-}

suma :: Nat -> Nat -> Nat
suma m = foldNat Suc m

{-
producto :: Nat -> Nat -> Nat
producto _ Cero = Cero
producto x (Suc y) = suma x (producto x y)
-}

producto :: Nat -> Nat -> Nat
producto n = foldNat (\x -> suma x n) Cero

{-
Ejercicio 6.6
Dado el tipo de datos 'Expr', funciones que calculan el número
de operadores en una expresión, su valor y el número de constantes
que aparecen en dicha expresión.
-}

data Expr = Valor Integer | Expr :+: Expr | Expr :-: Expr | Expr :*: Expr
			deriving Show
			
numOperadores :: Expr -> Integer
numOperadores (Valor _) = 0
numOperadores (e1 :+: e2) = numOperadores e1 + 1 + numOperadores e2
numOperadores (e1 :-: e2) = numOperadores e1 + 1 + numOperadores e2
numOperadores (e1 :*: e2) = numOperadores e1 + 1 + numOperadores e2

valorExpr :: Expr -> Integer
valorExpr (Valor x) = x
valorExpr (e1 :+: e2) = valorExpr e1 + valorExpr e2
valorExpr (e1 :-: e2) = valorExpr e1 - valorExpr e2
valorExpr (e1 :*: e2) = valorExpr e1 * valorExpr e2

numConstantes :: Expr -> Integer
numConstantes (Valor _) = 1
numConstantes (e1 :+: e2) = numConstantes e1 + numConstantes e2
numConstantes (e1 :-: e2) = numConstantes e1 + numConstantes e2
numConstantes (e1 :*: e2) = numConstantes e1 + numConstantes e2

{-
Ejercicio 6.7
Dado el tipo 'SnocList' que representa listas, definicion de varias
funciones para operar con ellas.
-}

infixl 5 :<
data SnocList a = Vacia | (SnocList a) :< a deriving Show

l :: SnocList Integer
l = Vacia :< 1 :< 2 :< 3 -- [1,2,3]

-- primer elemento de la lista

cabeza :: SnocList a -> a
cabeza Vacia = error "lista vacia"
cabeza (Vacia :< x) = x
cabeza (ls :< _) = cabeza ls

-- último elemento de la lista

ultimo :: SnocList a -> a
ultimo Vacia = error "lista vacia"
ultimo (_ :< x) = x

-- longitud de la lista

longitud :: SnocList a -> Integer
longitud Vacia = 0
longitud (ls :< _) = 1 + longitud ls

--- operador que concatena dos listas

(+++) :: SnocList a -> SnocList a -> SnocList a
(+++) ls Vacia = ls
(+++) ls (xs :< x) = ((+++) ls xs) :< x

-- map para snocList

mapSnocList :: (a -> b) -> SnocList a -> SnocList b
mapSnocList _ Vacia = Vacia
mapSnocList f (xs :< x) = (mapSnocList f xs) :< (f x)

{-
Ejercicio 6.8
Dado el tipo 'ConcatList' que representa listas, definicion de varias
funciones para operar con ellas.
-}

infixl 5 :+-+:
data ConcatList a = V | U a | ConcatList a :+-+: ConcatList a deriving Show

listaConcat :: ConcatList Integer
listaConcat = U 1 :+-+: U 2 :+-+: U 3 -- [1,2,3]

-- primer elemento de la lista

cabezaConcat :: ConcatList a -> a
cabezaConcat V = error "lista vacia"
cabezaConcat (U x) = x
cabezaConcat (x :+-+: _) = cabezaConcat x

-- último elemento de la lista

ultimoConcat :: ConcatList a -> a
ultimoConcat V = error "lista vacia"
ultimoConcat (U x) = x
ultimoConcat (_ :+-+: x) = ultimoConcat x

-- longitud de la lista

longitudConcat :: ConcatList a -> Integer
longitudConcat V = 0
longitudConcat (U _) = 1
longitudConcat (xs :+-+: ys) = longitudConcat xs + longitudConcat ys


-- operador para concatenar dos listas

(++++) :: ConcatList a -> ConcatList a -> ConcatList a
(++++) xs V = xs
(++++) xs (U y) = (xs :+-+: U y)
(++++) xs (ys :+-+: U y) = ((++++) xs ys) :+-+: U y

-- map para ConcatList

mapConcatList :: (a -> b) -> ConcatList a -> ConcatList b
mapConcatList _ V = V
mapConcatList f (U x) = U (f x)
mapConcatList f (xs :+-+: U x) = (mapConcatList f xs) :+-+: U (f x)

{-
Ejercicio 7.1
Tipo para representar números racionales.
-}

infix 9 :/
data Racional = Integer :/ Integer

-- La desigualdad se infiere a partir de ==
instance Eq Racional where
	(a :/ b) == (c :/ d) = (a * d) == (b * c)

-- Implementar compare o (<=) es suficiente
instance Ord Racional where
	compare (a :/ b) (c :/ d)
		| (a :/ b) == (c :/ d) = EQ
		| (a * c) <= (b * d) = LT
		| otherwise = GT
		
-- Aprovechamos que Integer ya es instancia de Show
instance Show Racional where
	show (a :/ b) = show a ++  "/" ++ show b

-- Operaciones de suma, resta y multiplicación	
instance Num Racional where
	(+) (a :/ b) (c :/ d) = (((a * d) + (c * b)) :/ (b * d))
	(-) (a :/ b) (c :/ d) = (((a * d) - (c * b)) :/ (b * d))
	(*) (a :/ b) (c :/ d) = ((a * c) :/ (b * d))
	abs (a :/ b) = ((abs a) :/ (abs b))
	signum (a :/ b) = (a :/ b) -- ?
	fromInteger x = (x :/ 1)
	
instance Fractional Racional where
	(/) (a :/ b) (c :/ d) = ((a * d) :/ (b * c))
	fromRational a = (1 :/ 2) -- ¿?
	
{-
Ejercicio 7.2
Hacer el tipo 'Nat' (definido más arriba) instancia de Eq, Ord y Num.
-}

instance Eq Nat where
	Cero == Cero = True
	(Suc x) == (Suc y) = x == y
	_ == _ = False

instance Ord Nat where
	compare Cero Cero = EQ
	compare Cero (Suc y) = LT
	compare (Suc x) Cero = GT
	compare (Suc x) (Suc y) = compare x y
	
instance Num Nat where
	(+) a b = suma a b
	(-) a b = resta a b
	(*) a b = producto a b
	abs a = a
	signum a = a -- ?
	fromInteger x = intANat x
	
{-
Ejercicio 7.3
Hacer los tipos 'SnocList' y 'ConcatList' (definidos más arriba) instancias
de Functor. Functor generaliza el comportamiento de 'map' para listas.
-}

-- data SnocList a = Vacia | (SnocList a) :< a deriving Show
instance Functor SnocList where
	fmap f Vacia = Vacia
	fmap f (xs :< x) = (fmap f xs) :< (f x)

-- data ConcatList a = V | U a | ConcatList a :+-+: ConcatList a deriving Show
instance Functor ConcatList where
	fmap = mapConcatList

{-
Ejercicio 7.4
Clase para medir valores de diferentes tipos.
-}

class Medible a where
	tamano :: a -> Integer

instance Medible Char where
	tamano x = 1
	
instance Medible Bool where
	tamano x = 1
	
instance Medible Integer where
	tamano x = 1
	
instance Medible Double where
	tamano x = 1

instance Medible Float where
	tamano x = 1
	
{- TODO :	
instance Medible  ¿?¿?¿? where
	tamano [] = 0
	tamano (x:xs) = tamano x + tamano xs
-}

{-
Ejercicio 7.5
Función ocurrencias, que toma un elemento y una lista y devuelve
el número de ocurrencias del elemento en la lista.
-}

ocurrencias :: Eq a => a -> [a] -> Integer
ocurrencias _ [] = 0
ocurrencias x (y:ys)
	| x == y = 1 + ocurrencias x ys
	| otherwise = ocurrencias x ys
	
pertenece2 :: Eq a => a -> [a] -> Bool
pertenece2 x ls = if (ocurrencias x ls) == 0 then False else True

{-
Ejercicio 7.6
Definición del tipo de datos Color.
-}

data Color = Violeta | Azul | Verde | Amarillo | Naranja | Rojo 
			deriving (Show, Ord)
			
instance Eq Color where
	Violeta == Violeta = True
	Azul == Azul = True
	Verde == Verde = True
	Amarillo == Amarillo = True
	Naranja == Naranja = True
	Rojo == Rojo = True
	_ == _ = False
	
{-
Ejercicio 8.1
Operaciones sobre listas.
-}

-- parte una lista en dos mitades

partir :: [a] -> ([a],[a])
partir [] = ([],[])
partir[x] = ([x],[])
partir (x:y:zs) = (x:xs,y:ys)
	where 
	(xs,ys) = partir zs
	
-- mezcla dos listas ordenadas en una tercera ordenada

mezcla :: Ord a => [a] -> [a] -> [a]
mezcla [] ly = ly
mezcla lx [] = lx
mezcla lx@(x:xs) ly@(y:ys)
	| x <= y = x : mezcla xs ly
	| otherwise = y : mezcla lx ys
	
-- mergeSort, método de ordenación por mezcla

ordenaMezcla :: Ord a => [a] -> [a]
ordenaMezcla [] = []
ordenaMezcla [x] = [x]
ordenaMezcla zs = mezcla (ordenaMezcla xs) (ordenaMezcla ys)
	where
	(xs,ys) = partir zs
	
{-
Ejercicio 8.2
Comprueba si una lista está ordenada ascendentemente.
-}

estaOrdenada :: Ord a => [a] -> Bool
estaOrdenada [] = True
estaOrdenada [x] = True
estaOrdenada (x:y:zs)
	|x <= y = estaOrdenada (y:zs)
	|otherwise = False
	
{-
Ejercicio 8.3
Inserción ordenada, ordenación por inserción.
-}

-- inserta un elemento en su posición adecuada en una lista ordenada

insertar :: Ord a => a -> [a] -> [a]
insertar x [] = [x]
insertar x (y:ys)
	|x <= y = x : (y:ys)
	|otherwise = y : insertar x ys
	
{-
-- ordena una lista por el método de ordenación por inserción

insertSort :: Ord a => [a] -> [a]
insertSort [] = []
insertSort [x] = [x]
insertSort (x:xs) = insertar x (insertSort xs)
-}

-- insertSort como una concreción de una función de plegado

foldrins :: Ord a => (a -> [a] -> [a]) -> [a] -> [a]
foldrins _ [] = []
foldrins f (x:xs) = f x (foldrins f xs)

insertSort :: Ord a => [a] -> [a]
insertSort = foldrins insertar

{-
Ejercicio 8.4
Operaciones sobre listas.
-}

-- equivalente a 'inits', devuelve los segmentos iniciales de una lista

inits2 :: [a] -> [[a]]
inits2 [] = [[]]
inits2 (x:xs) = [] : map (x :) (inits2 xs)

-- equivalente a 'tails', devuelve los segmentos finales de una lista

tails2 :: [a] -> [[a]]
tails2 [] = [[]]
tails2 ls@(x:xs) = ls : (tails2 xs)

-- equivalente a 'segs', devuelve los segmentos de datos consecutivos de una lista
{- ???????
segs2 :: [a] -> [[a]]
segs2 [] = [[]]
segs2 ls@(x:xs) = segs2 xs ++ tails2(inits2 ls)
-}

-- función que devuelva todos los subconjuntos de una lista
{- ?????
partes :: [a] -> [[a]]
partes [] == [[]]
partes (x:xs) = [x] : (map (x :) partes xs)
-}

{-
Ejercicio 8.5
Definición de algunas funciones predefinidas para listas.
-}
{-
TODO
'head': devuelve el primer elemento de la lista
'tail': devuelve todo excepto el primer elemento de la lista
'last': devuelve el último elemento de la lista
'init': devuelve todo excepto el último elemento de la lista
'take': devuelve una lista con los primeros x elementos que
		se le pasan com parámetro
'drop': devuelve una lista resultante de eliminar los x primeros
		elementos
'(!!)': devuelve la cabeza de la lista resultante de hacer un drop
'takeWhile': devuelve una lista con los elementos que cumplan cierta
			condición
'sum': devuelve el resultado de sumar todos los elementos de la lista
'product': devuelve el resultado de multiplicar todos los elementos de
			la lista
'maximum': devuelve el mayor elemento de la lista
'minimum': devuelve el menor elemento de la lista
'zip': dadas dos listas, devuelve una lista con duplas resultantes de
		combinar  los elementos en posiciones equivalentes en ambas
'unzip': dada una lista de duplas, devuelve una dupla de dos listas
		resultante de separar los elementos de las duplas
'zipWith': aplica cierta función a los elementos de dos listas tomándolos
			de dos en dos
-}

{-
Ejercicio 8.6
Definición de algunas funciones predefinidas como concreciones
de foldr y foldl.
-}

-- 'length'
length2 :: [a] -> Integer
length2 = foldr (\x xs -> 1 + xs) 0

-- 'map'

mapF :: (a -> b) -> [a] -> [b]
mapF f = foldr (\x xs -> (f x) : xs) []

-- 'filter p'

filterP :: (a -> Bool) -> [a] -> [a]
filterP p = foldr (\x xs -> if (p x) then x : xs else xs) []

-- '(++ ys)'
-- TODO

-- 'concat'

concat2 :: [[a]] -> [a]
concat2 = foldr (\x xs -> x ++ xs) []

-- 'unzip'

unzip2 :: [(a,b)] -> ([a],[b])
unzip2 = foldr (\(x,y) (xs,ys) -> (x:xs,y:ys)) ([],[])

{-
Ejercicio 8.7
Definición de algunas funciones predefinidas de la biblioteca List.
-}

-- 'nub' elimina todos los elementos replicados en una lista

nub2 :: Eq a => [a] -> [a]
nub2 [] = []
nub2 (x:xs) = if pertenece2 x xs then nub2 xs else x : nub2 xs

-- 'intersperse' intercala un elemento entre los elementos de la lista

intersperse2 :: a -> [a] -> [a]
intersperse2 _ [] = []
intersperse2 _ [x] = [x]
intersperse2 e (x:y:xs) = x : e : intersperse2 e (y : xs)

-- 'isPreffixOf' comprueba si una lista es prefijo de otra

esPrefijo :: Eq a => [a] -> [a] -> Bool
esPrefijo (_:xs) [] = False
esPrefijo [] _ = True
esPrefijo (x:xs) (y:ys) = (x == y) && esPrefijo xs ys

-- 'isSuffixOf' comprueba si una lista es sufijo de otra

esSufijo :: Eq a => [a] -> [a] -> Bool
esSufijo xs ys = esPrefijo (reverse xs) (reverse ys)

-- 'delete' elimina la primera ocurrencia de un dato de una lista

delete2 :: Eq a => a -> [a] -> [a]
delete2 _ [] = []
delete2 e (x:xs) = if (e == x) then xs else x : (delete2 e xs)

-- '(\\)' realiza la diferencia de listas

(\\\) :: Eq a => [a] -> [a] -> [a]
(\\\) [] _ = []
(\\\) xs [] = xs
(\\\) xs (y:ys)
	| pertenece2 y xs = (\\\) (delete2 y xs) ys
	| otherwise = (\\\) xs ys
	
{-
Ejercicio 8.8
Funciones definidas utilizando listar por compresión.
-}

-- En una lista de números positivos, reemplaza cada número n por n
-- copias de sí mismo

expandir :: [Integer] -> [Integer]
expandir xs = [x | x <- xs, y <- [1..x]]

-- Concatenación de una lista de listas en una sola lista
-- TODO : aplicar el operador ++ a todos los elementos de x
{-
concat3 :: [[a]] -> [a]
concat3 xs = [x | x <- xs]
-}

{-
Ejercicio 8.9
Funciones de desplegado, generan listas en vez de consumirlas.
-}

unfold :: (b -> Bool) -> (b -> a) -> (b -> b) -> b -> [a]
unfold p h t x = fun x
	where
		fun x
			| p x = []
			| otherwise = h x : fun (t x)

-- cuadradosDesdeHasta n m		[x^2 | 	x <- [n...m]]

cuadradosDesdeHasta :: Integer -> Integer -> [Integer]
cuadradosDesdeHasta n m
	| n <= m = unfold (> m) (^2) (+1) n
	| otherwise = error "n debe ser menor o igual que m"
	
-- siendo n el argumento de entrada, devuelve la lista [n...]

desde :: Integer -> [Integer]
desde n = unfold (\x -> False) (\y -> y) (+1) n

-- función identidad [a] -> [a]

identidad :: Eq a => [a] -> [a]
identidad xs = unfold (\ls -> (ls == [])) head tail xs

-- función map

mapUnfold :: Eq a => (a -> b) -> [a] -> [b]
mapUnfold f xs = unfold (\ls -> (ls == [])) (f . head) tail xs

{-
Ejercicio 9.1
Ejercicios sobre Árboles Binarios.
-}

data ArbolB a = VacioB | NodoB (ArbolB a) a (ArbolB a) deriving Show

-- comprueba si todos los elementos del árbol cumplen cierta condición

todosArbolB :: (a -> Bool) -> ArbolB a -> Bool
todosArbolB _ VacioB = True
todosArbolB p (NodoB i r d) = todos3 (todosArbolB p i) r (todosArbolB p d)
	where
		todos3 soli x sold = (p x) && soli && sold
		
-- comprueba si alguno de los elementos del árbol cumple cierta condición

algunoArbolB :: (a -> Bool) -> ArbolB a -> Bool
algunoArbolB _ VacioB = False
algunoArbolB p (NodoB i r d) = alguno3 (algunoArbolB p i) r (algunoArbolB p d)
	where
		alguno3 soli x sold = (p x) || soli || sold
		
{-
Ejercicio 9.2
Considerando un tipo de árbol binario que sólo guarda información en sus
hojas, algunas funciones para operar con él.
-}

data ArbolH a = HojaH a | NodoH (ArbolH a) (ArbolH a) deriving Show

-- devuelve el número de nodos hoja del árbol

tamanoH :: ArbolH a -> Integer
tamanoH (HojaH a) = 1
tamanoH (NodoH i d) = tamanoH i + tamanoH d

-- devuelve la profundida del árbol

profundidadH :: ArbolH a -> Integer
profundidadH (HojaH a) = 1
profundidadH (NodoH i d) = mayor(1 + profundidadH i) (1 + profundidadH d)

-- comprueba si todos los elementos del árbol cumplen cierta condición

todosArbolH :: (a -> Bool) -> ArbolH a -> Bool
todosArbolH p (HojaH x) = p x
todosArbolH p (NodoH i d) = (todosArbolH p i) && (todosArbolH p d)

		
-- comprueba si alguno de los elementos del árbol cumple cierta condición

algunoArbolH :: (a -> Bool) -> ArbolH a -> Bool
algunoArbolH p (HojaH x) = p x
algunoArbolH p (NodoH i d) = (algunoArbolH p i) || (algunoArbolH p d)

-- instancia de la clase Functor
{-
instance Functor ArbolH where
	fmap f (HojaH a) = HojaH (f a)
	fmap f (NodoH i d) = (NodoH (fmap f i) (fmap f d))
-}

{-
Ejercicio 9.3
Considerando una función de plegado sobre ArbolH, definición de algunas
funciones sobre dicho plegado.
-}

foldArbolH :: (b -> b -> b) -> (a -> b) -> (ArbolH a -> b)
foldArbolH f g = fun
	where
		fun (HojaH x) = g x
		fun (NodoH i d) = f (fun i) (fun d)
		
-- TODO : definición equivalente sin utilizar where

-- tamanoH utilizando foldArbolH

tamanoH2 :: ArbolH a -> Integer
tamanoH2 = foldArbolH (+) (\x -> 1)

-- profundidadH utilizando foldArbolH

profundidadH2 :: ArbolH a -> Integer
profundidadH2 = foldArbolH (\xs ys -> mayor (1+xs) (1+ys)) (\x -> 1)

-- todosH utilizando foldArbolH

todosH2 :: (a -> Bool) -> ArbolH a -> Bool
todosH2 p = foldArbolH (&&) (\x -> p x)

-- algunoH utilizando foldArbolH

algunoH2 :: (a -> Bool) -> ArbolH a -> Bool
algunoH2 p = foldArbolH (||) (\x -> p x)

-- instancia de Functor utilizando foldArbolH
-- TODO : ???
{-
instance Functor ArbolH where
	fmap f = foldArbolH (\i d -> (NodoH i d)) f
-}

{-
Ejercicio 9.4
Instancias de Functor para árbol binarios y generales
usando las funciones de plegado.
-}

foldArbolB :: (b -> a -> b -> b) -> b -> (ArbolB a -> b)
foldArbolB f e = fun 
	where 
		fun VacioB = e
		fun (NodoB i r d) = f (fun i) r (fun d)

instance Functor ArbolB where
	fmap f VacioB = VacioB
	fmap f (NodoB i r d) = NodoB (fmap f i) (f r) (fmap f d)

-- Árbol general

data Arbol a = Vacio | Nodo a [Arbol a] deriving Show

foldArbol :: (a -> [b] -> b) -> b -> (Arbol a -> b)
foldArbol f e = fun
	where
		fun Vacio = e
		fun (Nodo x xs) = f x (map fun xs)
		
instance Functor Arbol where
	fmap f Vacio = Vacio
	fmap f (Nodo x xs) = Nodo (f x) (map (fmap f) xs)

{-
Ejercicio 9.5
Función 'todos' para árboles binarios y generales utilizando
las funciones de plegado.
-}

-- Árbol binario
		
todosArbolBfold :: (a -> Bool) -> ArbolB a -> Bool
todosArbolBfold p = foldArbolB (\i r d -> i && (p r) && d) True

-- Árbol General

todosArbol :: (a -> Bool) -> Arbol a -> Bool
todosArbol p = foldArbol (\x xs -> (p x) &&  (and xs)) True

{-
Ejercicio 9.6
Instancias de la clase 'TieneMaximo' para árbol general, binario
y árbol H.
-}

-- 'maximo' devuelve el mayor elemento de una estructura de datos t
class TieneMaximo t where
	maximo :: Ord a => t a -> a

-- Árbol general

instance TieneMaximo Arbol where
	maximo Vacio = error "Arbol Vacio"
	maximo (Nodo x []) = x
	maximo (Nodo x xs)
		| x >= maximum (map maximo xs) = x
		| otherwise = maximum (map maximo xs)

-- Árbol binario

instance TieneMaximo ArbolB where
	maximo VacioB = error "Arbol Vacio"
	maximo (NodoB VacioB r VacioB) = r
	maximo (NodoB VacioB r d)
		| r >= maximo d = r
		| otherwise = maximo d
	maximo (NodoB i r VacioB)
		| r >= maximo i = r
		| otherwise = maximo i
	maximo (NodoB i r d)
		| (r >= (maximo i)) && (r >= maximo d) = r
		| (maximo i) >= (maximo d) = maximo i
		| otherwise = maximo d
		
-- Árbol H :: data ArbolH a = HojaH a | NodoH (ArbolH a) (ArbolH a) deriving Show

instance TieneMaximo ArbolH where
	maximo (HojaH x) = x
	maximo (NodoH i d)
		| maximo i >= maximo d = maximo i
		| otherwise = maximo d	
	
{-
Ejercicio 9.7
Instancias de la clase 'Ocurre' para listas y los tres tipos de árboles.
-}

-- 'ocurre' devuelve las ocurrencias de un elemento en la estructura
-- 	de datos t

class Ocurre t where
	ocurre :: Eq a => a -> t a -> Integer
	
-- Árbol general

instance Ocurre Arbol where
	ocurre x Vacio = 0
	ocurre x (Nodo y ys)
		| x == y = 1 + sum(map (ocurre x) ys)
		| otherwise = sum(map (ocurre x) ys)

-- Árbol binario

instance Ocurre ArbolB where
	ocurre x VacioB = 0
	ocurre x (NodoB i r d)
		| x == r = 1 + (ocurre x i) + (ocurre x d)
		| otherwise = (ocurre x i) + (ocurre x d)
				
-- Árbol H :: data ArbolH a = HojaH a | NodoH (ArbolH a) (ArbolH a) deriving Show

instance Ocurre ArbolH where
	ocurre x (HojaH h) = if x == h then 1 else 0
	ocurre x (NodoH i d) = (ocurre x i) + (ocurre x d)	

{-
Ejercicio 9.8
Instancias de la clase 'Hoja' para los tres tipos de árboles.
-}

-- 'hojas' devuelve las hojas de la estructura de datos t
class Hoja t where
	hojas :: t a -> [a]

-- Árbol general

instance Hoja Arbol where
	hojas (Nodo x []) = [x]
	hojas (Nodo _ xs) = concat (map hojas xs) 
	
-- Árbol binario :: data ArbolB a = VacioB | NodoB (ArbolB a) a (ArbolB a) deriving Show

instance Hoja ArbolB where
	hojas VacioB = []
	hojas (NodoB VacioB r VacioB) = [r]
	hojas (NodoB i r d) = hojas i ++ hojas d

-- Árbol H

instance Hoja ArbolH where
	hojas (HojaH x) = [x]
	hojas (NodoH i d) = hojas i ++ hojas d

{-
Ejercicio 9.9
Función que elimina un dato de un Árbol AVL, de forma que el árbol
siga siendo AVL.
-}

-- TODO : no inserta bien, asociatividad a la derecha
insertarABB :: Ord a => a -> ArbolB a -> ArbolB a
insertarABB x VacioB = NodoB VacioB x VacioB
insertarABB x (NodoB i r d)
	| x < r = insertarABB x i
	| otherwise = insertarABB x d

altura :: ArbolB a -> Integer
altura VacioB = 0
altura (NodoB i r d) = 1 + mayor (altura i) (altura d)

esAVL :: ArbolB a -> Bool
esAVL VacioB = True
esAVL (NodoB i r d) = abs(altura i - altura d) <= 1