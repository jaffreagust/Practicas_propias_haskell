import Data.List
{-
consultar sobre el 4f de la ultima practica , el 3 de la ultima practica
4i de la primera complementaria

iniciar::Estado a
update:: Name -> a -> Estado a -> Estado a
lookFor :: Name -> Estado a -> Maybe a

--ejemplo Def name1 3(Def name2 4 E)
type name = [char]                        
data Estado a = E | Def Name a (Estado a) Name a
                ->Single Name a
iniciar = E
type Estado a = [(Name,a)]

-}
--EJERCICIO 3 ULTIMA PRACTICA-- 
type Name = [Char]                        
data Estado a = E | Def Name a (Estado a) deriving Show

--ejemplo Def name1 3(Def name2 4 E)

update n v E = Def n v E
update n v (Def m v2 r) | m==n = Def n v r
                        | otherwise = Def m v2 (update n v r)

lookFor n E = Nothing
lookFor n (Def m v r)  | m==n = Just v
                       | otherwise = lookFor n r

free n E = E
free n (Def m v1 r) | m==n = r
                   | otherwise = Def m v1 (free n r) 


--EJERCICIO 4F ULTIMA PRACTICA--

--Caso general de merge :

-- merge h1@(N _ x a1 b1) h2@(N _ y a2 b2) = if x<= y then makeH x a1 (merge b1 h2)

{-
--similar a merge, sin embargo si son iguales saca el elemento igual y mergea 
merge' h1@(N _ x a1 b1) h2@(N _ y a2 b2) =| x<y = makeH x a1 (merge' b1 h2)
                                          | x>y = makeH y a2 (merge' h1 b2)
                                          | otherwise = merge' (merge' a1 b1) h2

--Separa el x (como un nodo vacio en sus extremos) y aplica el nuevo merge a "a" y "b"
cleanup (N k x a b) | merge (N 1 x E E) (merge a b)

-}
type Rank = Int
data Heap a = Em | N Rank a (Heap a){-L-} (Heap a){-R-} deriving Show -- Rank = Dato a guarda

--similar a merge, sin embargo si son iguales saca el elemento igual y mergea 
merge' h1 Em = cleanup h1
merge' Em h2 = cleanup h2
merge' h1@(N _ x a1 b1) h2@(N _ y a2 b2) =| x<y = makeH x a1 (merge' b1 h2)
                                          | x>y = makeH y a2 (merge' h1 b2)
                                          | otherwise = merge' (merge' a1 b1) h2

--Separa el x (como un nodo vacio en sus extremos) y aplica el nuevo merge a "a" y "b"
cleanup (N k x a b) | merge' (N 1 x E E) (merge' a b)
