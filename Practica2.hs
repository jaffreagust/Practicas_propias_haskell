import Data.List
--EJERCICIO 1

--Sin record--
data Color = RGB Int Int Int deriving Show

mezclar (RGB x y z) (RGB x1 y1 z1) = (RGB (div (x+x1) 2) (div (y+y1) 2) (div (z+z1) 2) ) 

--Con record
data Color_reg = Color_reg{
                 r::Int,
                 g::Int,
                 b::Int
                }deriving Show


mezclaralt (Color_reg r g b) (Color_reg r1 g1 b1) = (Color_reg (div (r+r1) 2) (div (g+g1) 2) (div (b+b1) 2))


--EJERCICIO 2--

--Con data y registro

data Linea = Linea {
            charac::[Char],
            pos::Int
            }deriving Show

vacia = (Linea "" 0)

--Notese que no se piensa si pos es mayor al largo de la cadena,
-- ya que ese no es inconveniente del calculo principal, de la logica de la funcion
moverIzq (Linea a pos) | (pos == 0) = (Linea a pos)
                       | otherwise = (Linea a (pos-1)) 

moverDer (Linea a pos) | (pos == length a) = (Linea a pos)
                       | otherwise = (Linea a (pos+1)) 

moverIni (Linea a pos) = (Linea a 0)

moverFin (Linea a pos) = (Linea a (length a))

--Insertar v1--
f char a pos= (take pos a) ++ [char] ++ (drop pos a)
insertarv1 char (Linea a pos) = (Linea (f char a pos) (pos+1))

--Insertar v2--
insertarv2 char (Linea a 0) = (Linea (char : a) 1)
insertarv2 char (Linea a pos) = (Linea (insertar char a pos) (pos+1))

insertar char a 1 = char:a
insertar char (x:xs) pos = x:insertar char xs (pos-1)

--Borrar v1--
borrarv1 (Linea a pos) = (Linea ((take (pos-1) a) ++ (drop pos a)) (pos-1))

borrarv2 (Linea a pos) = (Linea (bor a pos) (pos-1))

bor (x:xs) 1 = xs  
bor (x:xs) pos = x: bor xs (pos-1)  

--EJERCICIO 2 CON TYPES

type Cadena = [Char]
type Cursor = Int

--"Holaquetal" = ("aloh","quetal")
type Linea1 = (Cadena,Cadena)


--Mover izq--
moverIzq1 ("",xs) = ("",xs)
moverIzq1 ((x:xs),ys) = (xs,x:ys)

--Mover der--
moverDer1 (xs,"") = (xs,"")
moverDer1 (xs,(y:ys)) = (y:xs,ys)

--MoverIni--
moverIni1 (xs,ys) = ("",reverse xs ++ ys)

--MoverFin--
moverFin1 (xs,ys) = (reverse ys ++ xs , "")

--Insertar--
insertar1 char (xs,ys) = (char:xs,ys)

--Borrar --
borrar1 ((x:xs),ys) = (xs,ys) 


--EJERCICIO 3--
data CList a = EmptyCL | CUnit a | Consnoc a (CList a) a deriving Show

headCL (CUnit x) = x
headCL (Consnoc x _ _) = x

tailCL (CUnit x) = EmptyCL
tailCL (Consnoc x (EmptyCL) y) = CUnit y
tailCL (Consnoc x xs z) = (Consnoc (headCL xs) (tailCL xs) z)

isEmptyCL EmptyCL = True
isEmptyCL _ = False

isCUnit (CUnit x)= True
isCUnit _ = False

reverseCL (EmptyCL) = EmptyCL
reverseCL (CUnit x) = CUnit x
reverseCL (Consnoc x xs y) = (Consnoc y (reverseCL xs) x)

--Ayuda con listas--
{-inits [x] = [[],[x]]
inits xs = inits (borrarU xs) ++ [[xs]]

lasts [x] = [[],[x]]
lasts xs = lasts (tail xs) ++ [[xss]]

borrarU [] = []
borrarU [x] = []
borrarU (x:xs) = x: borrarU xs -}
--Ejercicio 3c--
--Ejercicio 3d--
--Ejercicio 3e--

--EJERCICIO 4--
data Aexp= Num Int| Prod Aexp Aexp | Div Aexp Aexp deriving Show

eval:: Aexp->Int
eval (Prod  (Num x) (Num y) ) = x*y
eval (Div (Num x) (Num 0)) = error "No se puede dividir por cero"
eval (Div (Num x) (Num y)) = div x y


seval:: Aexp-> Maybe Int

seval (Prod  (Num x) (Num y) ) =Just (x*y)
seval (Div (Num x) (Num 0)) = Nothing
seval (Div (Num x) (Num y)) =Just (div x y)





--EJERCICIO 5--
data BST a = E | N (BST a)  a  (BST a) deriving Show

--a) (BST) --
maximun::BST a -> a

maximun (N izq x E) = x
maximun (N izq x der) = maximun der

minimun :: BST a -> a
minimun (N E x der) = x
minimun (N izq x der) = minimun izq


--b) (BST) --
checkBST (E) = True 
checkBST (N E x E) = True
checkBST (N E x r) = (minimun r) >= x && checkBST r
checkBST (N l x E) = (maximun l) <=x && checkBST l
checkBST (N l x r) =  (maximun l) <= x && checkBST l && (minimun r) >= x && checkBST r



--EJERCICIO 6--
data Tree a = Empty | Nodo (Tree a) a (Tree a) deriving Show



--a) (BST) --
completo:: a->Int -> Tree a

completo x 0 = Empty
--Aca ↓ la recursion doble hace que consuma mucha mas memoria, por lo tanto el sharing es malo
--completo a x = N (completo a (x-1))  (completo a (x-1)) 

--Aca↓ hay menos recursiones ya que se completa directamente una vez y posteriormente se reemplaza por su valor
--Consumiendo menos memoria, haciendo menos recursiones y maximizando el sharing
completo x d = let n = (completo x (d-1))
                   in Nodo n x n



--b) (BST)--
balanceado :: a->Int -> Tree a
balanceado x 0 = Empty
balanceado x n |  even(n-1) = let m = div (n-1) 2
                                  u = balanceado x m
                                  in Nodo u x u
               
               | otherwise = let m = div (n-1) 2 
                                 (t1,t2) = balanceado' x m
                                 in Nodo t1 x t2
                                    where balanceado' x m = (balanceado x (m+1), balanceado x m)




--EJERCICIO 7 --

member :: Ord a =>BST a -> a ->a -> Bool

member E x aux = (x == aux)
member (N l b r) x aux | (x>b) = member r x aux
                       | otherwise = member l x b



fromOrdListBST :: [a] -> BST a
{-elementos impares:

raiz: mitad de la lista (x!!div (length x) 2)
mitad inferior de la lista = take (div (length x) 2) x
mitad superior de la lista = drop (div ((length x) +1) 2) x

-}

fromOrdListBST []= E 
fromOrdListBST xs = let l = length xs
                        u = div l 2
                        m = xs!!u
                        zs = take u xs
                        ys = drop (u+1) xs
                        (t1,t2) = (fromOrdListBST zs, fromOrdListBST ys)
                        in   N t1 m t2



--EJERCICIO 8 (RBT)--
data Colors = R|B deriving Show
data RBT a = Em|T Colors (RBT a) a (RBT a) deriving Show

eq R R = True
eq B B = True
eq _ _ = False
--if ((truncate(logBase 2 (fromIntegral l)) `mod` 2) /= 0)
--Anotacion : La raiz siempre debe ser negra, esto porque si fuese roja rompería la invariante local (todo nodo rojo DEBE tener un padre negro)
fromOrdListRBT xs = if ((truncate(logBase 2 (fromIntegral (length xs))) `mod` 2) /= 0) then makeBlack (fromOrdListRBT' R xs)
                                                                                       else fromOrdListRBT' B xs


fromOrdListRBT' _ [] = Em
fromOrdListRBT' c xs = let l = length xs
                           u = div l 2
                           m = xs!!u
                           zs = take u xs
                           ys = drop (u+1) xs
                           (t1,t2) = (fromOrdListRBT' B zs, fromOrdListRBT' B ys)
                           (t3,t4) = (fromOrdListRBT' R zs, fromOrdListRBT' R ys)
                           in  if (eq c R) then T B t1 m t2
                                           else T R t3 m t4







--EJERCICIO 9 (RBT lbalance rbalance) --



--Inserta dato en una hoja, si esta repetido no lo inserta--
{-Factos
*El nodo se inserta como rojo para no romper la invariante global(2)
-}
insertRBT :: Ord a => a -> RBT a -> RBT a
insertRBT x t = makeBlack (ins x t)
    where ins x Em = T R Em x Em
          ins x (T c l y r) | x < y = balance c (ins x l) y r
                            | x > y = balance c l y (ins x r)
                            | otherwise = T c l y r

--Peruanizador--
makeBlack Em = Em
makeBlack (T _ l x r) = T B l x r



--Toma las 3 componentes de un arbol y retorna un arbol--

balance :: Colors -> RBT a -> a -> RBT a -> RBT a
balance B (T R (T R a x b) y c) z d = T R (T B a x b) y (T B c z d)
balance B (T R a x (T R b y c)) z d = T R (T B a x b) y (T B c z d)
balance B a x (T R (T R b y c) z d) = T R (T B a x b) y (T B c z d)
balance B a x (T R b y (T R c z d)) = T R (T B a x b) y (T B c z d)
balance c l a r = T c l a r







--EJERCICIO 10 (LEFTIST HEAP)--


type Rank = Int
data Heap a = Emp | Nod Rank a (Heap a){-L-} (Heap a){-R-} deriving Show -- Rank = Dato a guardar 

--Fusion entre dos Leftist Heap--
merge :: Ord a => Heap a -> Heap a -> Heap a
merge h1 Emp = h1
merge Emp h2 = h2
merge h1@(Nod _ x a1 b1) h2@(Nod _ y a2 b2) =
    if x <= y then makeH x a1 (merge b1 h2)
              else makeH y a2 (merge h1 b2)

--Extrae el rango
rank :: Heap a -> Rank
rank Emp = 0
rank (Nod r _ _ _) = r

--Consulta el rango de los dos nodos
-- Si es mas grande el rango de b se incrementa en uno por la raiz añadida
-- el mas grande pasara a ser la espina izquierda (se menciona primero), y la menor será la derecha
makeH x a b = if rank a >= rank b then Nod (rank b + 1) x a b
                                  else Nod (rank a + 1) x b a


fromList [] = Emp
fromList zs = let hs = map (\x -> Nod 1 x Emp Emp) zs
                  pares []= []
                  pares [x] = [x]
                  pares (x:y:xs) = merge x y : pares xs
                  g [] = Emp
                  g [h] = h
                  g ys = g(pares ys) 
                  in g hs

