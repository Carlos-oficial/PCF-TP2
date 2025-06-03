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

-- List of adventurers
data Adventurer = P1 | P2 | P5 | P10 deriving (Show,Eq)

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
changeState o s = \x -> if x == o then not (s o) else s x

-- Changes the state of the game for a list of objects 
mChangeState :: [Objects] -> State -> State
-- mChangeState os s = foldr changeState s os
mChangeState objs s o
  | o `elem` objs = not (s o)  -- se o objeto está na lista, inverte
  | otherwise     = s o       -- senão, mantém como estava

univ :: [Objects]
univ = [(Left P1),(Left P2),(Left P5), (Left P10),(Right ())]

adventurers = [P1,P2,P5, P10]

lantern = Right ()

leftSide :: State -> [Objects]
leftSide state = filter (not.state) univ

leftSideAdv state = filter(not.state.Left) adventurers 
rightSideAdv state = filter(state.Left) adventurers 

rightSide :: State -> [Objects]
rightSide state = filter state univ

safe :: State -> Bool 
safe s = all s univ

--- TASK 1 -------------------------------------------------------

-- Time that each adventurer takes to cross the bridge
getTimeAdv :: Adventurer -> Float
getTimeAdv P1 = 1
getTimeAdv P2 = 2
getTimeAdv P5 = 5
getTimeAdv P10 = 10

{-- 
 - For a given state of the game, the function presents 
 - all possible moves that the adventurers can make.  
--}
allValidPlaysList :: State -> [Duration State]
allValidPlaysList s | s lantern = [Duration(getTimeAdv p1 , (mChangeState [lantern,Left p1] s)) | p1 <- rightSideAdv s]
                                ++ [Duration((uncurry max) (getTimeAdv p1,getTimeAdv p2), (mChangeState [lantern,Left p1,Left p2] s)) | (p1,p2) <- makePairs $ rightSideAdv s] 
                | otherwise = [Duration(getTimeAdv p1 , (mChangeState [lantern,Left p1] s)) | p1 <- leftSideAdv s]
                                ++ [Duration((uncurry max) (getTimeAdv p1,getTimeAdv p2), (mChangeState [lantern,Left p1, Left p2] s)) | (p1,p2) <- makePairs $ leftSideAdv s]  

mAllValidPlaysList :: Duration State -> [Duration State]
mAllValidPlaysList s = (s *>) <$> (allValidPlaysList $ getValue s)

allValidPlays :: State -> ListDur State
allValidPlays = LD . allValidPlaysList


addLevel :: (a -> [a]) -> Tree a -> Tree a
addLevel f (Node x [])     = Node x [ Node l [] | l <- (f x)] 
addLevel f (Node x forest) = Node x $ (addLevel f) <$> forest 

bfs :: ((Duration State) -> Bool) -> Tree (Duration State) -> Bool
bfs goal stateTree = (any goal) ((last . levels) stateTree) || bfs goal (expand stateTree) 
  where expand = addLevel mAllValidPlaysList
  -- expand the state and map x*> to the result, adding the duration to it 

f = bfs (\(Duration (d,s)) -> d >= 10) (Node (Duration (0, gInit)) [])

bfsIO :: ((Duration State) -> Bool) -> Tree (Duration State) -> IO Bool
bfsIO goal stateTree = do
  let currentLevel = last (levels stateTree)
  putStrLn $ "Current level: " ++ show (map getValue currentLevel)

  let found = any goal currentLevel
  putStrLn $ "Goal found in current level? " ++ show found

  if found
    then return True
    else do
      let nextTree = addLevel (allValidPlaysList . getValue) stateTree
      putStrLn "Recursing to next level..."
      bfsIO goal nextTree


{-- 
 - For a given number n and initial state, the function calculates
 - all possible n-sequences of moves that the adventures can make 
--}
exec :: Int -> State -> ListDur State
exec 0 s = return s
exec n s = do s' <- allValidPlays s ; exec (n-1) s' 

{-- 
 - Is it possible for all adventurers to be on the other side
 - in <=17 min and not exceeding 5 moves ? 
--}
leq17 :: Bool
leq17 = any (\(Duration (d,s)) -> (safe s) && (d <= 17)) (remLD (exec 5 gInit))  --true in exec 17 initial state undefined

{-- Is it possible for all adventurers to be on the other side
 - in < 17 min ? 
--}
l17 :: Bool
l17 = any (\(Duration (d,s)) -> (safe s) && (d < 17)) (remLD (exec 17 gInit))  --true in exec 17 initial state undefined


--- END OF TASK 1 -------------------------------------------------

--- TASK 2 --------------------------------------------------------

-- Represents which adventurer(s) to move next
type Move = Either Adventurer (Adventurer, Adventurer)

-- Calculates the resulting state based on which adventurers to move
-- next and their probabilistic crossing times

uniform_plus_minus = uniform . (\(Duration (d,s)) -> [Duration (d-d/2,s),Duration (d,s),Duration (d+d/2,s)])
uniform_plus_minus_ :: Float -> Dist Float
uniform_plus_minus_ = uniform . (\d -> [d-d/2 , d , d+d/2])

play :: Move -> State -> DistDur State
play  (Left a) s = 
  let
    next_state = mChangeState [lantern, Left a] s
    time_dist  = uniform_plus_minus_ $ getTimeAdv a
    dist_state =  (\d -> Duration (d, next_state)) <$> time_dist
  in
    DD dist_state

play  (Right (a1,a2)) s = DD . uniform_plus_minus $ Duration (max (getTimeAdv a1) (getTimeAdv a2), (mChangeState [lantern,Left a1,Left a2] s))

-- Extends the previous function to lists of movements
plays :: [Move] -> State -> DistDur State
plays [] s = return s 
plays (m:ms) s = do s' <- plays ms s ; play m s' 

--- END OF TASK 2 -------------------------------------------------

--- MONAD IMPLEMENTATIONS -----------------------------------------

-- Non-determinism combined with durations
data ListDur a = LD [Duration a] deriving Show


remLD :: ListDur a -> [Duration a]
remLD (LD x) = x

instance Functor ListDur where
   fmap f = let f' = (fmap f) in
     LD . (map f') . remLD

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
