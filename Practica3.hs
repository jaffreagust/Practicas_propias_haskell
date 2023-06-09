data Nat = Cero | Succ Nat deriving Show

--1)a)--

--Succ :: Nat->Nat

--1)b) --

int2Nat :: Int -> Nat

int2Nat 0 = Cero
int2Nat x = Succ (int2Nat (x-1))


--1)c)--
sumaNat Cero x = x
sumaNat (Succ(y)) x = Succ(sumaNat y x)


--1)d)--
nat2Int Cero = 0
nat2Int (Succ(x)) = 1+ nat2Int x


--EJERCICIO 2--

--2)a)--
data Arb = Empty | Hoja Int | Nodo Arb Arb deriving Show

data Cmd = L|R deriving Show

--Nodo:: Arb->Arb->Arb

--2)b)-- 
selec :: [Cmd] -> Arb->Arb

selec [] arb = arb
selec [L] (Nodo l r) = l
selec [R] (Nodo l r) = r
selec (L:xs) arb = selec xs (selec [L] arb)
selec (R:xs) arb = selec xs (selec [R] arb)




--Ejemplo : (Nodo (Nodo (Hoja 3) (Hoja 2)) (Hoja 3))
--Se utiliza map debido a que el tipo que sale de enum es [[CMD]], mientras que L o R es solo CMD
enum''(Empty) = [[]]
enum'' (Hoja _) = [[]]
enum'' (Nodo izq Empty) = map (L:) (enum'' izq)
enum'' (Nodo Empty der) = map (R:) (enum'' der)
enum'' (Nodo izq der) = (map (L:) (enum'' izq)) ++ (map (R:) (enum'' der)) 


-- EJERCICIO 3--
type Name = [Char]                        
data Estado a = Empt | Def Name a (Estado a) deriving Show

--ejemplo Def name1 3(Def name2 4 E)
initiate = Empt

update n v Empt = Def n v Empt
update n v (Def m v2 r) | m==n = Def n v r
                        | otherwise = Def m v2 (update n v r)

lookFor n Empt = Nothing
lookFor n (Def m v r)  | m==n = Just v
                       | otherwise = lookFor n r

free n Empt = Empt
free n (Def m v1 r) | m==n = r
                    | otherwise = Def m v1 (free n r) 




--EJERCICIO 4--

--4a (BST)--
data Bin a = E | N (Bin a) a (Bin a) deriving (Show,Eq)

cantNodos E d = 0
cantNodos (N l x r) 0 = 1
cantNodos (N l x r) d = (cantNodos l (d-1))+ (cantNodos r (d-1))

--4b (BST)
altura h E = [h]
altura h (N l x r) = altura (h+1) l ++ altura (h+1) r

checkBalanceBTS (N l x r) =( minimum (altura 0 (N l x r)) +1) >= maximum (altura 0 (N l x r))



--4c) (BST)--
minimun E  = 0
minimun (N E x r)  = x
minimun (N l x r)  = minimun l 

maximun E  = 0
maximun (N l x E)  = x
maximun (N l x r)  = maximun r 


sucPredBTS' y E z1 z2 = (z1, z2)
sucPredBTS' y (N l x r) z1 z2 | (x==y) = (maximun l , minimun r)
                             | (x>y) = sucPredBTS' y l z1 x
                             | (x<y) = sucPredBTS' y r x z2

sucPredBTS y arb = sucPredBTS' y arb 0 0




--4d (LEFTIST HEAP)--
type Rank = Int
data Heap a = Em | Nod Rank a (Heap a){-L-} (Heap a){-R-} deriving Show -- Rank = Dato a guardar 


orden [] = []
orden (x:xs) = [u| u<- orden xs , u<=x] ++ [x] ++ [i | i<- orden xs, i>x]

inorden Em = []
inorden (Nod _ x l r) = inorden l ++ [x] ++ inorden r 

ordLeftist arb = orden(inorden (arb)) 



--4e (LEFTIST HEAP)--
--METODO 1--

minL (Nod _ x Em Em) = x 
minL (Nod _ x Em r) = min x (minL r)
minL (Nod _ x l Em) = min x (minL l)
minL (Nod _ x l r) = min x (min (minL l) (minL r))


--Agregue la condicion de que el rango del padre tiene que ser igual al rango del hijo derecho más 1, asi se evitan problemas en el rango
checkLHeap' Em = True
checkLHeap' (Nod c x Em Em) = c == 1
checkLHeap' t@(Nod c x l Em) = let m = minL t
                                   in x == m && checkLHeap' l && c == 1
checkLHeap' t@(Nod c x Em r) = let m = minL t
                                   in x == m && checkLHeap' r && c == (rank r)+1 
checkLHeap' t@(Nod c x l r) = let m = minL t
                                  lc = rank l
                                  rc = rank r
                                  in x == m && lc >= rc && checkLHeap' l && checkLHeap' r && c==rc+1



--METODO 2--

findMin Em = 0
findMin (Nod _ x _ _) = x

checkLHeap Em = True
checkLHeap (Nod c x Em Em) = c==1
checkLHeap (Nod c x l Em) = x<findMin(l) && c==1 && checkLHeap l 
checkLHeap (Nod c x Em r) = x<findMin(r) && c == (rank r) +1 && checkLHeap r 
checkLHeap (Nod c x l r) = x<findMin(l) && x<findMin(r) && c==(rank r) +1 && rank l >= rank r  &&checkLHeap l && checkLHeap r 





--4f (LEFTIST HEAP))--
--Fusion entre dos Leftist Heap--
merge :: Ord a => Heap a -> Heap a -> Heap a
merge h1 Em = h1
merge Em h2 = h2
merge h1@(Nod _ x a1 b1) h2@(Nod _ y a2 b2) =
    if x <= y then makeH x a1 (merge b1 h2)
              else makeH y a2 (merge h1 b2)

--Extrae el rango
rank :: Heap a -> Rank
rank Em = 0
rank (Nod r _ _ _) = r

--Consulta el rango de los dos nodos
-- Si es mas grande el rango de b se incrementa en uno por la raiz añadida
-- el mas grande pasara a ser la espina izquierda (se menciona primero), y la menor será la derecha
makeH x a b = if rank a >= rank b then Nod (rank b + 1) x a b
                                  else Nod (rank a + 1) x b a




--MEDIANTE TRANSFORMACION EN LISTA , EXTRACCION DE REPETIDOS E INSERCION COMO LISTA

fromList [] = Em
fromList xs = let hs = map (\x -> Nod 1 x Em Em) xs
                  pares [] = []
                  pares [x] = [x]
                  pares (x:y:ys) = merge x y : pares ys
                  g [] = Em
                  g [h] = h
                  g ys = g (pares ys)
                  in g hs

onlyonex x [] = []
onlyonex x (y:ys) = if (x==y) then onlyonex x ys
                              else y:onlyonex x ys

unique [] = []
unique (x:xs) = x:(onlyonex x (unique xs))

notdupLHeap heap = fromList (unique (inorden heap)) 




--MEDIANTE CAMBIO EN LA FUNCION MERGE Y EXTRACCION DE LA RAIZ CON MERGEO RECURSIVO DE LOS NODOS 

--similar a merge, sin embargo si son iguales saca el elemento igual y mergea 
merge' h1 Em = cleanup h1
merge' Em h2 = cleanup h2
merge' h1@(Nod _ x a1 b1) h2@(Nod _ y a2 b2)  | x<y = makeH x a1 (merge' b1 h2)
                                              | x>y = makeH y a2 (merge' h1 b2)
                                              | otherwise = merge' (merge' a1 b1) h2

--Separa el x (como un nodo vacio en sus extremos) y aplica el nuevo merge a "a" y "b"
cleanup (Nod k x a b) = merge' (Nod 1 x Em Em) (merge' a b)







--4g (RBT)--

data Color = Re | B deriving Show
data RBT a= Emp | T Color (RBT a) a (RBT a) deriving Show --E = empty T = Nodo--

altB Emp = 0
altB (T B l _ r) = min (1 + altB l) (1 + altB r )
altB (T Re l _ r) = min (altB l) (altB r )


--Chequear que el arbol sea BST--
minimunRBT (T _ Emp x r) = x
minimunRBT (T _ l x r) = minimunRBT l  

maximunRBT (T _ l x Emp) = x
maximunRBT (T _ l x r) = maximunRBT r 


checkBSTRBT Emp = True
checkBSTRBT (T _ Emp x Emp) = True
checkBSTRBT (T _ l x Emp) = (maximunRBT l) <=x && checkBSTRBT l
checkBSTRBT (T _ Emp x r) = (minimunRBT r) >=x && checkBSTRBT r 
checkBSTRBT (T _ l x r) = (maximunRBT l) <=x && checkBSTRBT l &&  (minimunRBT r) >=x && checkBSTRBT r 


--funcion semidefinitiva, aplica todas las comparaciones necesarias para saber si es RBT y BST
checkRBT' Emp = True
checkRBT' (T _ Emp x Emp) = True
checkRBT' (T Re (T Re _ y _) x _ ) = False
checkRBT' (T Re _ x (T Re _ y _) ) = False
checkRBT' arb@(T _ Emp x r) = altB r == 0 && checkRBT' r && (checkBSTRBT arb)
checkRBT' arb@(T _ l x Emp) = altB l == 0 && checkRBT' l && (checkBSTRBT arb)
checkRBT' arb@(T _ l x r) = altB l == altB r && checkRBT' l && checkRBT' r && (checkBSTRBT arb)

--Funcion definitiva, chequea que el primer nodo sea negro
checkRBT (T Re _ _ _) = False
checkRBT arb = checkRBT' arb



