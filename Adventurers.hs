{-# LANGUAGE FlexibleInstances #-}
module Adventurers where

import System.IO
import DurationMonad
import Probability
import Data.List (singleton)
import Data.Ord (comparing)
import Data.Tree
import qualified DurationMonad
import DurationMonad (getValue)
import Data.Either (Either(Left))
import Data.Semigroup (Semigroup)
import Text.Read (Read(..), Lexeme(Ident), lexP, parens, readPrec,pfail)

-- List of adventurers
data Adventurer = P1 | P2 | P5 | P10 deriving (Show,Eq)

instance Read Adventurer where
  readPrec = parens $ do
    Ident s <- lexP
    case s of
      "P1"  -> return P1
      "P2"  -> return P2
      "P5"  -> return P5
      "P10" -> return P10
      _     -> pfail

-- Adventurers + the lantern
type Objects = Either Adventurer ()


{-- 
 - State of the game, i.e. the current position of each adventurer
 - + the lantern. The function (const False) represents the initial state of the
 - game, i.e. all adventurers + the lantern on the left side of the bridge.  The
 - function (const True) represents the end state of the game, i.e. all
 - adventurers + the lantern on the right side of the bridge.  
--}

type State = Objects -> Bool

instance Show State where
  show s = show  (zipWith (\x y -> x ++ (if y then " R" else " L")) ["p1","p2","p5","p10","l"] sh)
    where sh = map s univ


instance Ord State where
  compare = comparing show

instance Eq State where
  (==) s1 s2 = and [s1 (Left P1) == s2 (Left P1),
                    s1 (Left P2) == s2 (Left P2),
                    s1 (Left P5) == s2 (Left P5),
                    s1 (Left P10) == s2 (Left P10),
                    s1 (Right ()) == s2 (Right ())]

-- Initial state of the game
gInit :: State
gInit = const False

-- Changes the state s of the game for a given object o
changeState :: Objects -> State -> State
changeState o s = \x -> if x == o then not (s x) else s x

-- Changes the state of the game for a list of objects 
mChangeState :: [Objects] -> State -> State
mChangeState os s = foldr changeState s os
-- mChangeState objs s o
--   | o `elem` objs = not (s o)  -- se o objeto está na lista, inverte
--   | otherwise     = s o       -- senão, mantém como estava

univ :: [Objects]
univ = [(Left P1),(Left P2),(Left P5), (Left P10),(Right ())]

adventurers :: [Objects]
adventurers = Left <$> [P1,P2,P5, P10]

lantern :: Objects
lantern = Right ()

leftSide :: State -> [Objects]
leftSide state = filter (not.state) univ

rightSide :: State -> [Objects]
rightSide state = filter state univ

leftSideAdv :: State -> [Objects]
leftSideAdv state = filter (not.state) adventurers 

rightSideAdv :: State -> [Objects]
rightSideAdv state = filter state adventurers 

safe :: State -> Bool 
safe s = all s univ

-- Represents which adventurer(s) to move next
type Move = Either Adventurer (Adventurer, Adventurer)

--- MONAD IMPLEMENTATIONS -----------------------------------------

-- Non-determinism combined with durations
data ListDur a = LD [Duration a] deriving Show


remLD :: ListDur a -> [Duration a]
remLD (LD x) = x

instance Functor ListDur where
   fmap f = let f' = (fmap f) in
     LD . (map f') . remLD

instance Semigroup (ListDur a) where
  a <> b = LD (remLD a ++ remLD b) 

instance Monoid (ListDur a) where
  mempty = LD []

instance Applicative ListDur where
   pure x = LD [Duration (0,x)]
   l1 <*> l2 = LD $ do x <- remLD l1
                       y <- remLD l2
                       return $ do f <- x; a <- y; return (f a)

instance Monad ListDur where
   return = pure
   l >>= k = LD $ do x <- remLD l
                     g x where
                       g(Duration (i,x)) = let u = (remLD (k x))
                          in map (\(Duration (i',x)) -> Duration (i + i', x)) u

manyChoice :: [ListDur a] -> ListDur a
manyChoice = LD . concat . (map remLD)

-- Probabilistic behaviour combined with durations (note the similarity
-- with the previous code)
data DistDur a = DD (Dist (Duration a)) deriving Show

remDD :: DistDur a -> Dist (Duration a)
remDD (DD x) = x

instance Functor DistDur where
        fmap f = DD . (fmap (fmap f)) . remDD

instance Applicative DistDur where
        pure x = DD (return $ return x)
        d1 <*> d2 = DD $ do x <- remDD d1
                            y <- remDD d2
                            return $ do f <- x; a <- y; return (f a)

instance Monad DistDur where
        return = pure
        d >>= k = DD $ do x <- remDD d
                          g x where
                          g(Duration (i,x)) = let u = (remDD (k x))
                                in fmap (\(Duration (i',x)) -> Duration (i + i', x)) u


--- END OF MONAD IMPLEMENTATIONS ----------------------------------

--------- LIST UTILS ----------------------------------------------

makePairs :: (Eq a) => [a] -> [(a,a)]
makePairs as = normalize $ do a1 <- as; a2 <- as; [(a1,a2)]
                                
normalize :: (Eq a) => [(a,a)] -> [(a,a)]
normalize l = removeSw $ filter p1 l where
  p1 (x,y) = (/=) x y

removeSw :: (Eq a) => [(a,a)] -> [(a,a)]
removeSw [] = []
removeSw ((a,b):xs) = if elem (b,a) xs then removeSw xs else (a,b):(removeSw xs)

-- double map / map^2
(<$$>) :: (Functor f1, Functor f2) => (a -> b) -> f1 (f2 a) -> f1 (f2 b)
(<$$>) = ((<$>) . (<$>))
