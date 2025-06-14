module Tasks where

import Adventurers

import System.IO
import DurationMonad
import Probability

import Control.Monad
import Data.List (singleton,sort)
import Data.Ord (comparing)
import Data.Tree
import qualified DurationMonad
import DurationMonad (getValue, getDuration)
import Data.Either (Either(Left))
import Data.Foldable (concatMap)


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

addLevel :: (a -> [a]) -> Tree a -> Tree a
addLevel f (Node x [])     = Node x [ Node l [] | l <- (f x)] 
addLevel f (Node x forest) = Node x $ (addLevel f) <$> forest 

bfs :: ((Duration State) -> Bool) -> Tree (Duration State) -> Bool
bfs goal stateTree = (any goal) ((last . levels) stateTree) || bfs goal (expand stateTree) 
  where expand = addLevel mAllValidPlaysList

f = bfs (\(Duration (d,s)) -> d >= 10) (Node (Duration (0, gInit)) [])

bfsDebug :: ((Duration State) -> Bool) -> Tree (Duration State) -> IO Bool
bfsDebug goal stateTree = do
  let currentLevel = last (levels stateTree)
  putStrLn $ "Current level: " ++ show (map getValue currentLevel)

  let found = any goal currentLevel
  putStrLn $ "Goal found in current level? " ++ show found

  if found
    then return True
    else do
      let nextTree = addLevel (allValidPlaysList . getValue) stateTree
      putStrLn "Recursing to next level..."
      bfsDebug goal nextTree


{-- 
 - For a given number n and initial state, the function calculates
 - all possible n-sequences of moves that the adventures can make 
--}

exec :: Int -> State -> ListDur State
-- exec n = sequ [allValidPlays | x <- [1..n]]
exec 0 s = return s 
exec n s = do s' <- exec (n-1) s ; allValidPlays s' 

exec' :: Int -> State -> ListDur State
exec' 0 s = return s
exec' n s = do s' <- allValidPlays s ; exec' (n-1) s'

exec_acum 0 s = return s
exec_acum n s = do s' <- allValidPlays s ; return s <> exec' (n-1) s' 

exec_time_limit :: Float -> Duration State -> ListDur State
exec_time_limit limit s = 
     let ss  = mAllValidPlaysList s
         ss' = filter ((<= limit) . getDuration) ss
     in
        if ss' == [] 
        then LD $ singleton s
        else LD $ (\d'@(Duration(d,x)) -> remLD $ exec_time_limit (limit) d') =<< ss'

{-- 
 - Is it possible for all adventurers to be on the other side
 - in <=17 min and not exceeding 5 moves ? 
--}

leq17 :: Bool
leq17 = any (\(Duration (d,s)) -> (safe s) && (d <= 17)) (remLD (exec_acum 5 gInit))  --true in exec 17 initial state undefined
-- leq17 = any (\(Duration (d,s)) -> (safe s) && (d <= 17)) ( remLD $ exec_time_limit 17 (return gInit))

{-- Is it possible for all adventurers to be on the other side
 - in < 17 min ? 
--}
l17 :: Bool
l17 = any (\(Duration (d,s)) -> (safe s) && (d < 17)) (remLD (exec_acum 8 gInit)) -- true in exec 17 initial state undefined
-- l17 = any (\(Duration (d,s)) -> (safe s) && (d < 17)) ( remLD $ exec_time_limit 17 (return gInit))

--- END OF TASK 1 -------------------------------------------------


--- TASK 2 --------------------------------------------------------

-- Calculates the resulting state based on which adventurers to move
-- next and their probabilistic crossing times

uniform_plus_minus' = uniform . (\(Duration (d,s)) -> [Duration (d-d/2,s),Duration (d,s),Duration (d+d/2,s)])

uniform_plus_minus :: Fractional a => a -> Dist a
uniform_plus_minus = uniform . (\d -> [d-d/2 , d , d+d/2])

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

-- plays [] s = return s 
-- plays (m:ms) s = do s' <- plays ms s ; play m s' 

--- END OF TASK 2 -------------------------------------------------
