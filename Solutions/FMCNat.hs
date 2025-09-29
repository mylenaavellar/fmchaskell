{-# LANGUAGE GADTs #-}

module ExNat where

-- Do not alter this import!
import Prelude
    ( Show(..)
    , Eq(..)
    , Ord(..)
    , Num(..)
    , Integral(..)
    , Bool(..) , not , (&&) , (||)
    , ($)
    , (.)
    , (++)
    , undefined
    , error
    , otherwise
    )

-- some sugar
zero, one, two, three, four, five, six, seven, eight :: Nat
zero  = O
one   = S zero
two   = S one
three = S two
four  = S three
five  = S four
six   = S five
seven = S six
eight = S seven

-- Define evenerything that is undefined,
-- without using standard Haskell functions.
-- (Hint: recursion is your friend!)

data Nat where
  O :: Nat
  S :: Nat -> Nat

----------------------------------------------------------------
-- typeclass implementations
----------------------------------------------------------------

instance Show Nat where

    -- zero  should be shown as O
    -- three should be shown as SSSO
    show O = "O"
    show (S n) = "S" ++ show n

instance Eq Nat where

    (==) = undefined

instance Ord Nat where

    (<=) = undefined

    -- Ord does not REQUIRE defining min and max.
    -- Howevener, you should define them WITHOUT using (<=).
    -- Both are binary functions: max m n = ..., etc.

    min = undefined

    max = undefined


----------------------------------------------------------------
-- internalized predicates
----------------------------------------------------------------

isZero :: Nat -> Bool
isZero O = True
isZero (S _) = False

-- pred is the predecessor but we define zero's to be zero
pred :: Nat -> Nat
pred O = O
pred (S n) = n

even :: Nat -> Bool
even O = True
even (S O) = False
even (S (S n)) = even n

odd :: Nat -> Bool
odd O = False
odd (S O) = True
odd (S (S n)) = odd n


----------------------------------------------------------------
-- operations
----------------------------------------------------------------

-- addition
(<+>) :: Nat -> Nat -> Nat
n <+> O   = n
n <+> S m = S (n <+> m)

-- This is called the dotminus or monus operator
-- (also: proper subtraction, arithmetic subtraction, ...).
-- It behaves like subtraction, except that it returns 0
-- when "normal" subtraction would return a negative number.
monus :: Nat -> Nat -> Nat
O `monus` _ = O
n `monus` O = n
(S n) `monus` (S m) = monus n m

(<->) :: Nat -> Nat -> Nat
(<->) = monus

-- multiplication
times :: Nat -> Nat -> Nat
_ `times` O = O
n `times` (S m) = n `times` m + n

(<*>) :: Nat -> Nat -> Nat
(<*>) = times

-- power / exponentiation
pow :: Nat -> Nat -> Nat
_ `pow` O = S O
n `pow` (S m) = n `pow` m <*> n

exp :: Nat -> Nat -> Nat
exp = pow

(<^>) :: Nat -> Nat -> Nat
(<^>) = pow

-- quotient
(</>) :: Nat -> Nat -> Nat
_ </> O = undefined
O </> _ = O
n </> m = 
  case n <-> m of
    O ->
      case m <-> n of
        O -> S O
        _ -> O
    k -> S (k </> m)

-- remainder
(<%>) :: Nat -> Nat -> Nat
_ <%> O = undefined
n <%> m = n <-> (m <*> (n </> m))

-- euclidean division
eucdiv :: (Nat, Nat) -> (Nat, Nat)
eucdiv = undefined

-- divides
(<|>) :: Nat -> Nat -> Bool
O <|> _ = undefined
_ <|> O = True
n <|> m =
  case m <%> n of
    O -> True
    _ -> False

divides = (<|>)


-- distance between nats
-- x `dist` y = |x - y|
-- (Careful here: this - is the real minus operator!)
dist :: Nat -> Nat -> Nat
O `dist` m = m
n `dist` O = n
(S n)  `dist` (S m) = n `dist` m

(|-|) = dist

factorial :: Nat -> Nat
factorial O = S O
factorial (S n) = (S n) <*> factorial n

-- signum of a number (-1, 0, or 1)
sg :: Nat -> Nat
sg O = O
sg _ = S O

-- lo b a is the floor of the logarithm base b of a
lo :: Nat -> Nat -> Nat
lo O _ = undefined
lo _ O = undefined
lo _ (S O) = O
lo b a =
  case a </> b of
    O -> O
    k -> S (lo b k)


----------------------------------------------------------------
-- Num & Integral fun
----------------------------------------------------------------

-- For the following functions we need Num(..).
-- Do NOT use the following functions in the definitions above!

toNat :: Integral a => a -> Nat
toNat = undefined

fromNat :: Integral a => Nat -> a
fromNat = undefined


-- Voilá: we can now easily make Nat an instance of Num.
instance Num Nat where

    (+) = (<+>)
    (*) = (<*>)
    (-) = (<->)
    abs n = n
    signum = sg
    fromInteger x
      | x < 0     = undefined
      | x == 0    = undefined
      | otherwise = undefined

