import Data.List

data Bin a = Hoja | Nodo (Bin a) a (Bin a) deriving Show

member :: Eq a => a -> Bin a -> Bool
member a Hoja = False
member a (Nodo l b r ) = a == b || member a l || member a r


insertT :: Ord a => a -> Bin a -> Bin a
insertT a Hoja = Nodo Hoja a Hoja
insertT a (Nodo l b r ) | a <= b = Nodo (insertT a l) b r
                       | otherwise = Nodo l b (insertT a r )



--Cuando se refiere a log n se refiere a base 2--
{-
Se dice que un arbol se encuentra balanceado cuando la altura del arbol izquierdo
 y el arbol derecho de cada nodo difieren a lo sumo en 1
-}

--Un RBT tiene dos invariantes--
{-
(1)*Todo nodo rojo tiene un padre negro
(2)*Todo CAMINO desde la RAIZ hasta la HOJA tiene la MISMA CANTIDAD de nodos NEGROS
-}



data Color = R | B deriving Show
data RBT a= E | T Color (RBT a) a (RBT a) deriving Show --E = empty T = Nodo--

--(T B (T R (T B E 1 E) 2 (T B E 3 E)) 5 (T R (T B E 6 E) 7 (T B E 8 E)))


--Para cada tipo de "data" creado ante la necesidad de mostrarlo
--Necesito usar deriving Show

memberRBT :: Ord a => a -> RBT a -> Bool
memberRBT a E = False
memberRBT a (T _ l b r) | (a == b) = True
                        | a < b = memberRBT a l
                        | a > b = memberRBT a r


--Inserta dato en una hoja, si esta repetido no lo inserta--
{-Factos
*El nodo se inserta como rojo para no romper la invariante global(2)
-}
insertRBT :: Ord a => a -> RBT a -> RBT a
insertRBT x t = makeBlack (ins x t)
    where ins x E = T R E x E
          ins x (T c l y r) | x < y = balance c (ins x l) y r
                            | x > y = balance c l y (ins x r)
                            | otherwise = T c l y r

--Peruanizador--
makeBlack E = E
makeBlack (T _ l x r) = T B l x r



--Toma las 3 componentes de un arbol y retorna un arbol--

balance :: Color -> RBT a -> a -> RBT a -> RBT a
balance B (T R (T R a x b) y c) z d = T R (T B a x b) y (T B c z d)
balance B (T R a x (T R b y c)) z d = T R (T B a x b) y (T B c z d)
balance B a x (T R (T R b y c) z d) = T R (T B a x b) y (T B c z d)
balance B a x (T R b y (T R c z d)) = T R (T B a x b) y (T B c z d)
balance c l a r = T c l a r
--Costo LOGARITMICO--
--no recursivo--

--HEAPS (NO es un Binary Tree Search)--

{-una invariante
(1)* Todo nodo es menor al todos los valores de sus hijos
-}
 -- operaciones : Insert
                --Find minimum
                --Delete minimum
--LEFTIST HEAPS--
{-dos invariantes
(1)* Todo nodo es menor al todos los valores de sus hijos
(2)* el rango de cualquier hijo izquierdo es mayor o
igual que el de su hermano de la derecha
-}

{-Factos
*La espina derecha es la ruta más corta a una hoja.
*Los elementos de la espina derecha están ordenados.
-}

type Rank = Int
data Heap a = Em | N Rank a (Heap a){-L-} (Heap a){-R-} deriving Show -- Rank = Dato a guardar 

--Fusion entre dos Leftist Heap--
merge :: Ord a => Heap a -> Heap a -> Heap a
merge h1 Em = h1
merge Em h2 = h2
merge h1@(N _ x a1 b1) h2@(N _ y a2 b2) =
    if x <= y then makeH x a1 (merge b1 h2)
              else makeH y a2 (merge h1 b2)

--Extrae el rango
rank :: Heap a -> Rank
rank Em = 0
rank (N r _ _ _) = r

--Consulta el rango de los dos nodos
-- Si es mas grande el rango de b se incrementa en uno por la raiz añadida
-- el mas grande pasara a ser la espina izquierda (se menciona primero), y la menor será la derecha
makeH x a b = if rank a >= rank b then N (rank b + 1) x a b
                                  else N (rank a + 1) x b a



insert :: Ord a => a -> Heap a -> Heap a
insert x h = merge (N 1 x Em Em) h

findMin :: Ord a => Heap a -> a
findMin (N _ x a b) = x

deleteMin :: Ord a => Heap a -> Heap a
deleteMin (N _ x a b) = merge a b

