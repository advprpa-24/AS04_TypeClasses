module TypeClasses.Nat where

todo :: a
todo = error "TODO"

-- Data type for a natural number.
data Nat 
    = Z     -- Zero
    | S Nat -- Successor of a natural number
    deriving Show

zero :: Nat
zero = Z

one :: Nat
one = S Z

two :: Nat
two = S (S Z)

three :: Nat
three = S (S (S Z))

-- Addition
add :: Nat -> Nat -> Nat
add Z n     = n
add (S m) n = S (add m n)

{-
add (S (S Z)) (S Z)
~> S (add (S Z) (S Z))
~> S (S (add Z (S Z)))
~> S (S (S Z))
-}  

-- Saturated substraction (https://en.wikipedia.org/wiki/Saturation_arithmetic)
sub :: Nat -> Nat -> Nat
sub m Z     = m
sub (S m) (S n) = sub m n

-- Multiplication
mul:: Nat -> Nat -> Nat
mul Z _     = Z
mul (S m) n = add n (mul m n)

{- 
3 * 4
4 + (2 * 4)
4 + (4 + (1 * 4))
4 + (4 + (4 + 0))

4 + 4 + 4
-}

-- Conversion
-- Negative inputs map to Z
integerToNat :: Integer -> Nat
integerToNat n | n < 0 = Z
integerToNat 0 = Z
integerToNat n = S (integerToNat (n-1))

-- Conversion
natToInteger :: Nat -> Integer
natToInteger Z = 0
natToInteger (S n) = 1 + natToInteger n

-- Num instance allows to use common arithmetic operations when writing expressions of type Nat.
instance Num Nat where
  (+) :: Nat -> Nat -> Nat
  (+) = add

  (*) :: Nat -> Nat -> Nat
  (*) = mul

  (-) :: Nat -> Nat -> Nat
  (-) = sub

  -- Returns the argument unchanged
  negate :: Nat -> Nat
  negate = id

  -- Returns the argument unchanged
  abs :: Nat -> Nat
  abs = id

  -- Returns Z if arg is zero and one (S Z) if argument is positive.
  signum :: Nat -> Nat
  signum Z = Z
  signum _ = S Z

  fromInteger :: Integer -> Nat
  fromInteger = integerToNat


main :: IO ()
main = do
    print ((1 + 3 * 13 - 3 + 5) :: Nat)
    print $ natToInteger (1 + 3 * 13 - 3 + 5)
    
