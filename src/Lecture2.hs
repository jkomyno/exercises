{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE EmptyDataDecls #-}

{- |
Module                  : Lecture2
Copyright               : (c) 2021-2022 Haskell Beginners 2022 Course
SPDX-License-Identifier : MPL-2.0
Maintainer              : Haskell Beginners 2022 Course <haskell.beginners2022@gmail.com>
Stability               : Stable
Portability             : Portable

Exercises for the Lecture 2 of the Haskell Beginners course.

As in the previous section, implement functions and provide type
signatures. If the type signature is not already written, write the
most polymorphic type signature you can.

Unlike exercises to Lecture 1, this module also contains more
challenging exercises. You don't need to solve them to finish the
course but you can if you like challenges :)
-}

module Lecture2
    ( -- * Normal
      lazyProduct
    , duplicate
    , removeAt
    , evenLists
    , dropSpaces

    , Knight (..)
    , dragonFight

      -- * Hard
    , isIncreasing
    , merge
    , mergeSort

    , Expr (..)
    , Variables
    , EvalError (..)
    , eval
    , constantFolding
    ) where

import Data.Char (isSpace)

{- | Implement a function that finds a product of all the numbers in
the list. But implement a lazier version of this function: if you see
zero, you can stop calculating product and return 0 immediately.

>>> lazyProduct [4, 3, 7]
84
-}
lazyProduct :: [Int] -> Int
lazyProduct (0:_) = 0
lazyProduct (x:xs) = x * lazyProduct xs
lazyProduct [] = 1

{- | Implement a function that duplicates every element in the list.

>>> duplicate [3, 1, 2]
[3,3,1,1,2,2]
>>> duplicate "cab"
"ccaabb"
-}
duplicate :: [a] -> [a]
duplicate (x:xs) = x : x : duplicate xs
duplicate [] = []

{- | Implement function that takes index and a list and removes the
element at the given position. Additionally, this function should also
return the removed element.

>>> removeAt 0 [1 .. 5]
(Just 1,[2,3,4,5])

>>> removeAt 10 [1 .. 5]
(Nothing,[1,2,3,4,5])
-}
removeAt :: Int -> [a] -> (Maybe a, [a])
removeAt 0 (x:xs) = (Just x, xs)
removeAt n (x:xs) = let
  (maybeRes, remaining) = removeAt (n - 1) xs
  in (maybeRes, x : remaining)
removeAt _ [] = (Nothing, [])

{- | Write a function that takes a list of lists and returns only
lists of even lengths.

>>> evenLists [[1,2,3], [3,1,2,7], [], [5, 7, 2]]
[[3,1,2,7],[]]

♫ NOTE: Use eta-reduction and function composition (the dot (.) operator)
  in this function.
-}
evenLists :: [[a]] -> [[a]]
evenLists = let
  isEvenList = even . length
  in foldr (\x acc -> if isEvenList x then x : acc else acc) []

{- | The @dropSpaces@ function takes a string containing a single word
or number surrounded by spaces and removes all leading and trailing
spaces.

>>> dropSpaces "   hello  "
"hello"
>>> dropSpaces "-200            "
"-200"

♫ NOTE: As in the previous task, use eta-reduction and function
  composition (the dot (.) operator) in this function.

🕯 HINT: look into Data.Char and Prelude modules for functions you may use.
-}
dropSpaces :: String -> String
dropSpaces = let
  f = reverse . dropWhile isSpace
  in f . f

{- |

The next task requires to create several data types and functions to
model the given situation.

An evil dragon attacked a village of innocent citizens! After
returning to its lair, the dragon became hungry and ate one of its
treasure chests by accident.

The guild in the village found a brave knight to slay the dragon!
As a reward, the knight can take the treasure chest.

Below is the description of the fight and character specifications:

  * A chest contains a non-zero amount of gold and a possible treasure.
    When defining the type of a treasure chest, you don't know what
    treasures it stores insight, so your chest data type must be able
    to contain any possible treasure.
  * As a reward, knight takes all the gold, the treasure and experience.
  * Experience is calculated based on the dragon type. A dragon can be
    either red, black or green.
  * Red dragons grant 100 experience points, black dragons — 150, and green — 250.
  * Stomachs of green dragons contain extreme acid and they melt any
    treasure except gold. So green dragons has only gold as reward.
    All other dragons always contain treasure in addition to gold.
  * Knight tries to slay dragon with their sword. Each sword strike
    decreases dragon health by the "sword attack" amount. When the
    dragon health becomes zero or less, a dragon dies and the knight
    takes the reward.
  * After each 10 sword strikes, dragon breathes fire and decreases
    knight health by the amount of "dragon fire power". If the
    knight's health becomes 0 or less, the knight dies.
  * Additionally, each sword strike decreases "knight's endurance" by one.
    If knight's endurance becomes zero, they become tired and are not
    able to continue the fight so they run away.

Implement data types to describe treasure, knight and dragon.
And implement a function that takes a knight and a dragon and returns
one of the three possible fight outcomes.

You're free to define any helper functions.

🕯 HINT: If you find the description overwhelming to implement entirely
  from scratch, try modelling the problem in stages.

    1. Implement all custom data types without using polymorphism.
    2. Add @newtype@s for safety where you think is appropriate.
    3. Encode the fight result as a sum type.
    4. Add polymorphism.
    5. Make all invalid states unrepresentable. Think, how you can
       change your types to prevent green dragons from having any
       treasure besides gold (if you already haven't done this).
-}

-- some help in the beginning ;)
data Knight = Knight
    { knightHealth    :: Int
    , knightAttack    :: Int
    , knightEndurance :: Int
    }

newtype Experience = Experience Int

data Treasure

data Chest = Chest
    { chestGold     :: Int
    , chestTreasure :: Maybe Treasure 
    }

data DragonColor
    = Red
    | Black
    | Green
    deriving (Show, Eq)

data Dragon = Dragon
    { dragonColor            :: DragonColor
    , dragonHealth           :: Int
    , dragonFirePower        :: Int
    , dragonChest            :: Chest
    }

data FightResult
  = DragonWon
    { turn   :: Int
    , knight :: Knight
    , dragon :: Dragon
    }
  | KnightWon
    { turn       :: Int
    , knight     :: Knight
    , dragon     :: Dragon
    , experience :: Experience
    , chest      :: Chest
    }
  | KnightEscaped
    { turn   :: Int
    , knight :: Knight
    , dragon :: Dragon
    }

{- The fight is played in turns.
The knight starts by striking the dragon at most 10 times.
If the knight is not tired and the dragon is still alive, the dragon strikes back at the 11th turn.
Then, if the knight is still alive, they can strike again at most 10 times at the 12th turn, and so on.
Since the fight is strictly sequential, there cannot be ties (where both the knight and the player die).

The fight has several possible outcomes:
- @DragonWon@: the knight has been slain by the dragon.
- @KnightWon@: the dragon has been slain by the knight.
- @KnightEscaped@: the knight has run away because they were too exhausted.
-}
dragonFight :: Knight -> Dragon -> FightResult
dragonFight = dragonFight' 0

dragonFight' :: Int -> Knight -> Dragon -> FightResult
dragonFight' turn knight@Knight { knightHealth = 0 } dragon = DragonWon { turn, knight, dragon }
dragonFight' turn knight@Knight { knightEndurance = 0 } dragon = KnightEscaped { turn, knight, dragon }
dragonFight' turn knight dragon@Dragon { dragonHealth = 0 } = let
  Dragon { dragonColor, dragonChest } = dragon
  Chest { chestGold } = dragonChest
  experience = case dragonColor of
    Red -> Experience 100
    Black -> Experience 150
    Green -> Experience 250
  chest = case dragonColor of
    Green -> Chest { chestGold, chestTreasure = Nothing } -- the stomach of green dragons melts the treasure
    _     -> dragonChest
  in KnightWon { turn, knight, dragon, experience, chest }
dragonFight' turn knight dragon
    | turn `mod` 11 == 0 = let
      -- the dragon strikes back, inflicting a @dragonFirePower@ damage to the knight
      Knight { knightHealth } = knight
      Dragon { dragonFirePower } = dragon
      knight' = knight { knightHealth = max 0 (knightHealth - dragonFirePower) }
      in dragonFight' (turn + 1) knight' dragon
    | otherwise = let
      -- the knight strikes at most 10 times, inflicting a @knightAttack@ damage to the dragon,
      -- and decreasing the knight's @knightEndurance@ by one
      Dragon { dragonHealth } = dragon
      Knight { knightAttack, knightEndurance } = knight
      dragon' = dragon { dragonHealth = max 0 (dragonHealth - knightAttack) }
      knight' = knight { knightEndurance = knightEndurance - 1 }
      in dragonFight' (turn + 1) knight' dragon'

----------------------------------------------------------------------------
-- Extra Challenges
----------------------------------------------------------------------------

{- The following exercises are considered optional. Some of them might be more
challenging. However, you still may find some of them easier than some of the
previous ones. Difficulty is a relative concept.
-}

{- | Write a function that takes a list of numbers and returns 'True'
if all the numbers are in the increasing order (i.e. the list is
sorted).

>>> isIncreasing [3, 1, 2]
False
>>> isIncreasing [1 .. 10]
True
-}
isIncreasing :: [Int] -> Bool
isIncreasing (x:y:xs) = (x < y) && isIncreasing (y:xs)
isIncreasing [_] = True
isIncreasing [] = True

{- | Implement a function that takes two lists, sorted in the
increasing order, and merges them into new list, also sorted in the
increasing order.

The lists are guaranteed to be given sorted, so you don't need to
verify that.

>>> merge [1, 2, 4] [3, 7]
[1,2,3,4,7]
-}
merge :: [Int] -> [Int] -> [Int]
merge (x:xs) (y:ys) = if x <= y then x : merge xs (y:ys) else y : merge (x:xs) ys
merge [] ys = ys
merge xs [] = xs

{- | Implement the "Merge Sort" algorithm in Haskell. The @mergeSort@
function takes a list of numbers and returns a new list containing the
same numbers but in the increasing order.

The algorithm of merge sort is the following:

  1. If the given list has less than 2 elements, it's already sorted.
  2. Otherwise, split list into two lists of the same size.
  3. Sort each of two lists recursively.
  4. Merge two resulting sorted lists to get a new sorted list.

>>> mergeSort [3, 1, 2]
[1,2,3]
-}
mergeSort :: [Int] -> [Int]
mergeSort xs@(_:_:_) = let
  n = length xs
  mid = n `div` 2
  (left, right) = splitAt mid xs
  left' = mergeSort left
  right' = mergeSort right
  in merge left' right'
mergeSort [x] = [x]
mergeSort [] = []


{- | Haskell is famous for being a superb language for implementing
compilers and interpeters to other programming languages. In the next
tasks, you need to implement a tiny part of a compiler.

We're going to work on a small subset of arithmetic operations.

In programming we write expressions like "x + 1" or "y + x + 10".
Such expressions can be represented in a more structured way (than a
string) using the following recursive Algebraic Data Type:
-}
data Expr
    = Lit Int
    | Var String
    | Add Expr Expr
    deriving (Show, Eq)

{- Now, you can use this data type to describe such expressions:

> x + 1
Add (Var "x") (Lit 1)

> y + x + 10
Add (Var "y") (Add (Var "x") (Lit 10))
-}

{- | We want to evaluate such expressions. We can associate a value
with a variable using a list of pairs.

You can use the @lookup@ function to search in this list by a variable name:

 * https://hackage.haskell.org/package/base-4.16.0.0/docs/Prelude.html#v:lookup
-}
type Variables = [(String, Int)]

{- | Unfortunately, it's not guaranteed that variables in our @Expr@
data type are present in the given list. So we're going to introduce a
separate data for possible evaluation errors.

Normally, this would be a sum type with several constructors
describing all possible errors. But we have only one error in our
evaluation process.
-}
newtype EvalError
    = VariableNotFound String
    deriving (Show, Eq)

{- | Having all this set up, we can finally implement an evaluation function.
It returns either a successful evaluation result or an error.
-}
eval :: Variables -> Expr -> Either EvalError Int
eval _ (Lit n) = Right n
eval vars (Var name) = let
  maybeLit = lookup name vars
  in case maybeLit of
    Just lit -> Right lit
    Nothing  -> Left (VariableNotFound name)
eval vars (Add e1 e2) = let
  e1' = eval vars e1
  e2' = eval vars e2
  in case (e1', e2') of
    (Right n1, Right n2) -> Right (n1 + n2)
    (Left err1, _) -> Left err1
    (_, Left err2) -> Left err2

{- | Compilers also perform optimizations! One of the most common
optimizations is "Constant Folding". It performs arithmetic operations
on all constants known during compile time. This way you can write
more verbose and clear code that works as efficient as its shorter
version.

For example, if you have an expression:

x + 10 + y + 15 + 20

The result of constant folding can be:

x + y + 45

It also can be:

x + 45 + y

Write a function that takes and expression and performs "Constant
Folding" optimization on the given expression.
-}
constantFolding :: Expr -> Expr
constantFolding e@(Lit _)   = e
constantFolding e@(Var _)   = e
constantFolding (Add e1 e2) = let
  e1' = constantFolding e1
  e2' = constantFolding e2
  in case (e1', e2') of
    (Lit 0, _) -> constantFolding e2'
    (_, Lit 0) -> constantFolding e1'
    (Lit n1, Lit n2) -> constantFolding $ Lit (n1 + n2)
    (_, _) -> Add (constantFolding e1') (constantFolding e2')
