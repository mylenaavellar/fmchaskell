{-# LANGUAGE GADTs #-}

module FMCList where

import Prelude
    ( Char , String , Int , Integer , Double , Float , Bool(..)
    , Num(..) , Integral(..) , Enum(..) , Ord(..) , Eq(..)
    , not , (&&) , (||)
    , (.) , ($)
    , flip , curry , uncurry
    , otherwise , error , undefined
    )
import qualified Prelude   as P
import qualified Data.List as L
import qualified Data.Char as C
--import FMCNat

{- import qualified ... as ... ?

To use a function from a qualified import
you need to prefix its name with its alias and a dot:
P.head   C.toUpper   etc.

I import these for you to test the original functions on ghci:

ghci> :t C.toUpper
C.toUpper :: Char -> Char

You MUST NOT use ANY of these in your code

-}


{- Our lists vs Haskell lists

Our definition:

data List a where
  Nil  :: List a
  Cons :: a -> List a -> List a

Here we use Haskell's built-in lists and associated syntactic sugar.
It is as if it was defined like this:

    data [a] = [] | (x : xs)

or like this:

    data [a] where
      []  :: [a]
      (:) :: a -> [a] -> [a]

write [a]       for our List a
write []        for our List
write []        for our Nil
write (x : xs)  for our Cons x xs
write [u,v]     for our u `Cons` (v `Cons` Nil)

-}

head :: [a] -> a
head (x : _) = x
head [] = error "no head in nil"

tail :: [a] -> [a]
tail (_ : xs) = xs
tail [] = error "no tail in nil"

null :: [a] -> Bool
null [] = True
null _ = False

length :: Integral i => [a] -> i
length [] = 0
length (_ : xs) = 1 + length xs

sum :: Num a => [a] -> a
sum [] = 0
sum (x : xs) = x + sum xs

product :: Num a => [a] -> a
product [] = 1
product (x : xs) = x * product xs

reverse :: [a] -> [a]
reverse [] = []
reverse (x : xs) = reverse xs ++ [x]

(++) :: [a] -> [a] -> [a]
[] ++ xs = xs
(x : xs) ++ ys = x : (xs ++ ys)

-- right-associative for performance!
-- (what?!)
infixr 5 ++

-- (snoc is cons written backwards)
snoc :: a -> [a] -> [a]
snoc x [] = [x]
snoc x (y : ys) = y : snoc x ys

(<:) :: [a] -> a -> [a]
(<:) = flip snoc

-- different implementation of (++)
(+++) :: [a] -> [a] -> [a]
xs +++ []     = xs
xs +++ [y]    = xs <: y
xs +++ (y : ys) = (xs +++ [y]) +++ ys

-- left-associative for performance!
-- (hmm?!)
infixl 5 +++

minimum :: Ord a => [a] -> a
minimum [] = error "no minimum in nil"
minimum [x] = x
minimum (x : xs) = min x (minimum xs)

maximum :: Ord a => [a] -> a
maximum [] = error "no maximum in nil"
maximum [x] = x
maximum (x : xs) = max x (maximum xs)

take :: Int -> [a] -> [a]
take 0 _ = []
take _ [] = []
take n (x : xs) = x : take (n - 1) xs

drop :: Int -> [a] -> [a]
drop 0 xs = xs
drop _ [] = []
drop n (x : xs) = drop (n - 1) xs

-- takeWhile

-- dropWhile

tails :: [a] -> [[a]]
tails [] = [[]]
tails (x : xs) = (x : xs) : tails xs

init :: [a] -> [a]
init [] = error "no init in nil"
init [_] = []
init (x : xs) = x : init xs

inits :: [a] -> [[a]]
inits [] = [[]]
inits (x : xs) = [] : map (x :) (inits xs)

-- subsequences

-- any

-- all

and :: [Bool] -> Bool
and [] = True
and (x : xs) = x && and xs

or :: [Bool] -> Bool
or [] = False
or (x : xs) = x || or xs

concat :: [[a]] -> [a]
concat [] = []
concat (xs : ys) = xs ++ concat ys

-- elem using the function 'any' above

-- elem': same as elem but elementary definition
-- (without using other functions except (==))

-- (!!)

filter :: (a -> Bool) -> [a] -> [a]
filter _ [] = []
filter p (x : xs) =
  case p x of
    True  -> x : filter p xs
    False -> filter p xs

map :: (a -> b) -> [a] -> [b]
map _ [] = []
map f (x : xs) = f x : map f xs

cycle :: [a] -> [a]
cycle [] = error "no cycle in nil"
cycle xs = xs ++ cycle xs

repeat :: a -> [a]
repeat x = x : repeat x

replicate :: Int -> a -> [a]
replicate x n = take x (repeat n)

-- isPrefixOf
-- isInfixOf
-- isSuffixOf

-- zip
-- zipWith

-- intercalate
-- nub

-- splitAt
-- what is the problem with the following?:
-- splitAt n xs  =  (take n xs, drop n xs)

-- break

-- lines
-- words
-- unlines
-- unwords

-- transpose

-- checks if the letters of a phrase form a palindrome (see below for examples)
palindrome :: String -> Bool
palindrome xs =
  let normalized = normalize xs
  in length normalized > 1 && normalized == reverse normalized
  
-- normalize cleans the String so palindrome can work correctly
normalize :: String -> String
normalize = L.map C.toLower . L.filter C.isAlpha

{-

Examples of palindromes:

"Madam, I'm Adam"
"Step on no pets."
"Mr. Owl ate my metal worm."
"Was it a car or a cat I saw?"
"Doc, note I dissent.  A fast never prevents a fatness.  I diet on cod."

-}

