data Nat = Zero | Succ Nat

--Comparison operators
(==) :: Nat -> Nat -> Bool
(==) Zero Zero = True
(==) (Succ n) (Succ m) = n Main.== m
(==) n m = False

(/=) :: Nat -> Nat -> Bool
(/=) Zero Zero = False
(/=) (Succ n) (Succ m) = n Main./= m
(/=) n m = True

(>) :: Nat -> Nat -> Bool
(>) (Succ n) Zero = True
(>) Zero n = False
(>) (Succ n) (Succ m) = n Main.> m

(<) :: Nat -> Nat -> Bool
(<) Zero (Succ n) = True
(<) n Zero = False
(<) (Succ n) (Succ m) = n Main.< m

(>=) :: Nat -> Nat -> Bool
(>=) n m = n Main.== m || n Main.> m

(<=) :: Nat -> Nat -> Bool
(<=) n m = n Main.== m || n Main.< m

--Display operator
instance Show Nat where
    show :: Nat -> String
    show val = show (natToInt val)

--Arithmetic operators
(+) :: Nat -> Nat -> Nat
(+) n Zero = n
(+) Zero n = n
(+) n m
    | n Main.> m = add n m
    | otherwise = add m n
        where
            add x Zero = x
            add x (Succ y)
                | y Main.== Zero = Succ x
                | otherwise = add (Succ x) y

(-) :: Nat -> Nat -> Nat
(-) n Zero = n
(-) n m
    | n Main.> m = sub n m
    | n Main.<= m = Zero
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
        increment (Succ x) result = increment x (result Main.+ n)

(^) :: Nat -> Nat -> Nat
(^) _ Zero = Succ Zero
(^) Zero _ = Zero
(^) (Succ Zero) _ = Succ Zero
(^) n (Succ m) = mult m n
    where
        mult Zero result = result
        mult (Succ x) result = mult x (result Main.* n)
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
isZero = (Main.== Zero)

successor :: Nat -> Nat
successor = Succ

predecessor :: Nat -> Nat
predecessor Zero = Zero
predecessor (Succ x) = x
