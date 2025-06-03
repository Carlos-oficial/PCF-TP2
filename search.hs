class Search a where
  expand  :: a -> foldable a 
  enqueue :: foldable a -> a -> foldable a
  dequeue :: foldable a -> a
  goal    :: Ord x => a ->  x
  

