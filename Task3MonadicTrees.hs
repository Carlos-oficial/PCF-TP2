module Task3MonadicTrees where

import MonadTree
import Adventurers
import DurationMonad
import Probability
import Tasks
import Control.Monad


root :: Tree a f State
root = Leaf gInit

{-
f may be 
  - list
  - ListDur 
  - DistDur
-}

getRoot (Node n _) = n 
leavesListDur t = go t []

go :: Tree (Duration State) ListDur State -> [Duration State] -> [Duration State]
go (Leaf x) z = (return x):z
go (Node (Duration(d,_)) ts) z = (wait d) <$> (foldr go z l)
  where l = getValue <$> (remLD ts)


expandListDur :: State -> ListDur State
expandListDur = allValidPlays

expandDistDur :: State -> DistDur State
expandDistDur = undefined -- allValidPlays

lsStateTree :: Tree (Duration State) ListDur State
lsStateTree = f gInit 

f :: State -> Tree (Duration State) ListDur State
f s = do
  s' <- Node (return s) (Leaf <$> expandListDur s)
  f s' 

lsStateTreeN :: Int -> Tree (Duration State) ListDur State
lsStateTreeN n = f' n gInit
  where
    f':: Int -> State -> Tree (Duration State) ListDur State
    f' 0 s = return s
    f' n s = 
      do
        s' <- Node (return s) (Leaf <$> expandListDur s) 
        f' (n-1) s'

lsToListTree :: Tree (Duration State) ListDur State ->  Tree (Duration State) [] (Duration State)
lsToListTree (Leaf n) = return $ return n 
lsToListTree (Node n (LD ns)) = Node n $  (lsToListTree . getValue <$> ns) 


play_game ::  Tree (Duration State) ListDur State -> IO ()
play_game (Leaf n) = do putStrLn $ "leaf: " ++ show n
play_game (Node n (LD ns)) = 
  do
    putStrLn $ "Current State: " ++ (show ) n
    
    let durs = (getRoot . getValue <$> ns)
    let lines = (show <$>) (zip [0..] durs)
    
    putStrLn $ ("durations: \n" ++) $ unlines $ ("\t"++) <$> lines
    putStrLn "choose a branch to explore:"
    
    chosen_play :: Int <- readLn
    play_game (getValue (ns !! chosen_play)) 

g = play_game $ lsStateTree

