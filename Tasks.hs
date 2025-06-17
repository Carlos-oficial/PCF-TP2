module Tasks where

import Adventurers

import System.IO
import DurationMonad
import Probability

import Control.Monad
import Data.List (singleton,sort)
import Data.Ord (comparing)
import Data.Tree
import DurationMonad
import Data.Either (Either(Left))
import Data.Foldable (concatMap)
import DurationMonad (getDuration)

myShowList a = putStrLn $ unlines $ show <$> a
-- Time that each adventurer takes to cross the bridge
getTimeAdv :: Adventurer -> Float
getTimeAdv P1 = 1
getTimeAdv P2 = 2
getTimeAdv P5 = 5
getTimeAdv P10 = 10

sameSideAsLantern :: (Objects -> Bool) -> [Objects]
sameSideAsLantern state = filter (\x -> state x == state lantern) adventurers

{-- 
 - For a given state of the game, the function presents 
 - all possible moves that the adventurers can make.  
--}

allValidPlaysList :: State -> [Duration State]
allValidPlaysList s = 
            [
                Duration (
                    getTimeAdv p1 ,
                    mChangeState [lantern,Left p1] s
                    )
                | (Left p1) <- sameSideAsLantern s 
            ] ++ [
                Duration (
                    max (getTimeAdv p1) (getTimeAdv p2),
                    mChangeState [lantern,Left p1,Left p2] s
                    ) 
                | (Left p1, Left p2) <- makePairs $ sameSideAsLantern s
                ]

-- expand the state and map x*> to the result, adding the duration to it 
mAllValidPlaysList :: Duration State -> [Duration State]
mAllValidPlaysList s = (s *>) <$> (allValidPlaysList $ getValue s)

allValidPlays :: State -> ListDur State
allValidPlays = LD . allValidPlaysList

{-- 
 - For a given number n and initial state, the function calculates
 - all possible n-sequences of moves that the adventures can make 
--}

exec :: Int -> State -> ListDur State
exec 0 s = return s 
exec n s = do s' <- exec (n-1) s ; allValidPlays s' 

{--
  - Another possible implementation of exec
--}

exec' n = sequ [allValidPlays | x <- [1..n]]

{--
  a version of exec that mantains all achieved states, uses the <> operator,
  since ListDur is a monid
--}

exec_acum :: Int -> State -> ListDur State
exec_acum 0 s = return s
exec_acum n s = do s' <- allValidPlays s ; return s <> exec' (n-1) s' 

{-- 
 - Is it possible for all adventurers to be on the other side
 - in <=17 min and not exceeding 5 moves ? 
--}

leq17 :: Bool
leq17 = any (\(Duration (d,s)) -> (safe s) && (d <= 17)) (remLD (exec_acum 5 gInit))  --true in exec 17 initial state undefined

{-- Is it possible for all adventurers to be on the other side
 - in < 17 min ? 
--}

l17 :: Bool
l17 = any (\(Duration (d,s)) -> (safe s) && (d < 17)) (remLD (exec_acum 8 gInit)) -- true in exec 17 initial state undefined

--- END OF TASK 1 -------------------------------------------------


--- TASK 2 --------------------------------------------------------

uniform_plus_minus :: Fractional a => a -> Dist a
uniform_plus_minus = uniform . (\d -> [d-d/2 , d , d+d/2])

uniform_plus_minus' = uniform . (\(Duration (d,s)) -> [Duration (d-d/2,s),Duration (d,s),Duration (d+d/2,s)])

{- 
- Calculates the resulting state based on which adventurers to move
- next and their probabilistic crossing times
-}

play :: Move -> State -> DistDur State
play  (Left a) s = 
  let
    next_state = mChangeState [lantern, Left a] s
    time_dist = uniform_plus_minus $ getTimeAdv a
    dist_state = (\d -> Duration (d, next_state)) <$> time_dist
  in
    DD dist_state
play  (Right (a1,a2)) s = DD . uniform_plus_minus' $ Duration (max (getTimeAdv a1) (getTimeAdv a2), (mChangeState [lantern,Left a1,Left a2] s))

-- Extends the previous function to lists of movements
plays :: [Move] -> State -> DistDur State
plays l s = foldM (flip play) s l

-- Another implementation of plays
plays' [] s = return s 
plays' (m:ms) s = do s' <- plays' ms s ; play m s' 

--- END OF TASK 2 -------------------------------------------------
--- Task 3 ---

--- Trees ----

leaves t =
  let go (Node x []) z = x:z
      go (Node _ ts) z = foldr go z ts
  in go t []

leaf x = Node x []

{-
Here I explored in detail how to optimize task 1 with Data.Trees
-}

addLevel :: (a -> [a]) -> Tree a -> Tree a
addLevel f (Node x [])     = Node x [ Node l [] | l <- (f x)] 
addLevel f (Node x forest) = Node x $ (addLevel f) <$> forest 

bfs :: ((Duration State) -> Bool) -> Tree (Duration State) -> Bool
bfs goal stateTree = (any goal) ((last . levels) stateTree) || bfs goal (expand stateTree) 
  where expand = addLevel mAllValidPlaysList

{-
  expands an initial state to a Tree of states, computing the duration
  and stopping once a criterion is reached
  expandToTree stopping_criteria (Duration State)
-}

expandToTree ::  ((Duration State) -> Bool) -> (Duration State)  -> Tree (Duration State)
expandToTree c ds = Node ds $ ( expandToTree c ) <$> ( filter (not . c) $ mAllValidPlaysList ds )

l17' = any (safe.getValue) $ expandToTree ((>=17).getDuration) (return gInit)

{-
  `any` optimised with breadth first search
-}

bfsAny :: (a -> Bool) -> Tree a -> Bool
bfsAny p n = trav p [n]
  where
    trav :: (a -> Bool) -> [Tree a] -> Bool
    trav _ [] = False
    trav p ((Node x []) : q) = (p x) || (trav p q)
    trav p ((Node label st) : q) = (p label) || (trav p (q ++ st))

l17'' = bfsAny (safe . getValue) $ expandToTree (const False) (return gInit)

-- infinite search tree specific to the problem
infiniteSearchTree :: (Duration State)  -> Tree (Duration State)
infiniteSearchTree ds = Node ds $ infiniteSearchTree <$> ( mAllValidPlaysList ds )

l17''' = 
  bfsAny (\(Duration (d,s)) -> d <= 17 && safe s) $ 
    infiniteSearchTree' mAllValidPlaysList (return gInit)

-- more general generator for a infinite search tree
infiniteSearchTree' :: (a -> [a]) -> a -> Tree a
infiniteSearchTree' expand a = Node a $ (infiniteSearchTree' expand) <$> (expand a)

-- bfs search that knows when to stop
bfsAnyGiveUp :: (a -> Bool) -> (a -> Bool) -> Tree a -> Bool
bfsAnyGiveUp p g n = trav [n]
  where
    trav [] = False
    trav ((Node x []) : q) = (not $ g x) && ((p x) || trav q)

infiniteSearchAsUnfold expand = unfoldTree (\a -> (a, expand a))

infiniteSearchTree'' :: State  -> Tree (Duration State) 
infiniteSearchTree'' n =
  let initial :: Duration State = return n
  in 
    unfoldTree (\a -> (a, mAllValidPlaysList a)) initial
