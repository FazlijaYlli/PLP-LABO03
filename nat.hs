data Nat = Zero | Succ Nat
    deriving Eq

add :: Nat -> Nat -> Nat
add m Zero = m
add m (Succ n) = Succ (add m n)

natToInt :: Nat -> Int
natToInt Zero = 0
