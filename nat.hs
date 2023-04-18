{- 
    DEVOIR 3
    Auteurs : Ylli Fazlija, Rui Mota Carneiro
    Date : 18.04.23
    Description : Programme Haskell fournissant une implémentation des nombres naturels ainsi
    que des différents opérateurs pouvant être utilisés avec ceux-ci.
-}

data Nat = Zero | Succ Nat

--Comparison operators
instance Eq Nat where
    (==) :: Nat -> Nat -> Bool
    (==) Zero Zero = True
    (==) (Succ n) (Succ m) = n == m
    (==) n m = False

    (/=) :: Nat -> Nat -> Bool
    (/=) Zero Zero = False
    (/=) (Succ n) (Succ m) = n /= m
    (/=) n m = True

instance Ord Nat where
    (>) :: Nat -> Nat -> Bool
    (>) (Succ n) Zero = True
    (>) Zero n = False
    (>) (Succ n) (Succ m) = n > m

    (<) :: Nat -> Nat -> Bool
    (<) Zero (Succ n) = True
    (<) n Zero = False
    (<) (Succ n) (Succ m) = n < m

    (>=) :: Nat -> Nat -> Bool
    (>=) n m = n == m || n > m

    (<=) :: Nat -> Nat -> Bool
    (<=) n m = n == m || n < m

--Display operator
instance Show Nat where
    show :: Nat -> String
    show val = show (natToInt val)

--Arithmetic operators
instance Num Nat where
    (+) :: Nat -> Nat -> Nat
    (+) n Zero = n
    (+) Zero n = n
    (+) n m = successor n + predecessor m

    (-) :: Nat -> Nat -> Nat
    (-) n Zero = n
    (-) n m
        | n > m = sub n m
        | n <= m = Zero
            where
                sub x Zero = x
                sub (Succ x) (Succ y) = sub x y

    (*) :: Nat -> Nat -> Nat
    (*) _ Zero = Zero
    (*) Zero _ = Zero
    (*) n (Succ Zero) = n
    (*) n (Succ m) = increment m n
        where
            increment Zero result = result
            increment (Succ x) result = increment x (result + n)

(^) :: Nat -> Nat -> Nat
(^) _ Zero = Succ Zero
(^) Zero _ = Zero
(^) (Succ Zero) _ = Succ Zero
(^) n (Succ m) = mult m n
    where
        mult Zero result = result
        mult (Succ x) result = mult x (result * n)

--(5*4)^5 takes ~9 seconds
--(5*4)^6 takes ~1 minute to cause a stack overflow exception

--Functions
natToInt :: Nat -> Int
natToInt val = countToZero val 0
    where
        countToZero :: Nat -> Int -> Int
        countToZero Zero count = count
        countToZero (Succ n) count = countToZero n (count Prelude.+ 1)

intToNat :: Int -> Nat
intToNat val
    | val Prelude.<= 0 = Zero
    | otherwise = Succ (intToNat (val Prelude.- 1))

zero :: Nat
zero = Zero

isZero :: Nat -> Bool
isZero = (== Zero)

successor :: Nat -> Nat
successor = Succ

predecessor :: Nat -> Nat
predecessor Zero = Zero
predecessor (Succ x) = x
