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
data Bin a = E | N (Bin a) a (Bin a) deriving Show

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

minimun E z = z
minimun (N E x r) z = x
minimun (N l x r) z = minimun l

maximun E z = z
maximun (N l x R) z = x
maximun (N l x r) z = maximun r

sucPredBTS y (N l x r) | (x==y) = (maximun l y , minimun r y)
                       | (x>y) = sucPredBTS y l
                       | (x<y) = sucPredBTS y r
