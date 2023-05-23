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
insertarEnList a [] = []
insertarEnList a (x:xs) = (a:x):(insertarEnList a xs)


enum (Empty) = [[]]
enum (Hoja _) = [[]]
enum (Nodo izq Empty) = insertarEnList (L) (enum izq)
enum (Nodo Empty der) = insertarEnList (R) (enum der)
enum (Nodo izq der) = (insertarEnList (L) (enum izq)) ++ (insertarEnList (R) (enum der)) 

--EJERCICIO 4--

--4a)--
data Bin a = E | N (Bin a) a (Bin a) deriving (Show,Eq)

cantNodos E d = 0
cantNodos (N l x r) 0 = 1
cantNodos (N l x r) d = (cantNodos l (d-1))+ (cantNodos r (d-1))

--4b)
altura h E = [h]
altura h (N l x r) = altura (h+1) l ++ altura (h+1) r

checkBalanceBTS (N l x r) =( minimum (altura 0 (N l x r)) +1) >= maximum (altura 0 (N l x r))

--4c)--
{-treevalue y (N l x r) | (x==y) = (N l y r)
                      | (x<y) = treevalue y r 
                      | (x>y) = treevalue y l 

leftBranch (N l x r) = l
rightBranch (N l x r) = r

maximun (N izq x E) = x
maximun (N izq x der) = maximun der


minimun (N E x der) = x
minimun (N izq x der) = minimun izq

sucPredBTS y arb = (minimun(rightBranch(treevalue y arb)), maximun (leftBranch(treevalue y arb)))-}

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

--d)--
type Rank = Int
data Heap a = Em | Nod Rank a (Heap a){-L-} (Heap a){-R-} deriving Show -- Rank = Dato a guardar 


orden [] = []
orden (x:xs) = [u| u<- orden xs , u<=x] ++ [x] ++ [i | i<- orden xs, i>x]

inorden Em = []
inorden (Nod _ x l r) = inorden l ++ [x] ++ inorden r 

ordLeftist arb = orden(inorden (arb)) 

--e)--
checkLHeap Em = True
checkLHeap (Nod _ x Em Em) = True
checkLHeap (Nod _ x l@(Nod _ y _ _) Em) = (x<y) && checkLHeap l
checkLHeap (Nod _ x Em  r@(Nod _ y _ _)) = (x<y) && checkLHeap r
checkLHeap (Nod _ x l@(Nod r1 y _ _) r@(Nod r2 z _ _)) = (x < y) && (x < z) && checkLHeap l && checkLHeap r && r1 >= r2

--f)--
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

--g)--
data Color = Re | B deriving Show
data RBT a= Emp | T Color (RBT a) a (RBT a) deriving Show --E = empty T = Nodo--

altB Emp = 0
altB (T B l _ r) = min (1 + altB l) (1 + altB r )
altB (T Re l _ r) = min (altB l) (altB r )

checkRBT Emp = True
checkRBT (T _ Emp x r) = checkRBT r
checkRBT (T _ l x Emp) = checkRBT l
checkRBT (T Re (T Re _ y _) x _ ) = False
checkRBT (T Re _ x (T Re _ y _) ) = False
checkRBT (T _ l x r) = altB l == altB r && checkRBT l && checkRBT r


