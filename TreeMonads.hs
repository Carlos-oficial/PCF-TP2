module TreeMonads where

import Data.Tree
import Data.Monoid
import Control.Applicative
import Test.QuickCheck


{- (<>) :: Tree a -> Tree a -> Tree a
Node x [] <> ny = Node x (pure ny)
Node x fx <> ny = Node x ((TreeMonads.<> ny) <$> fx)
 -}

instance Semigroup a => Semigroup (Tree a) where
  Node a as <> Node b bs =
    Node (a <> b) (zipWith (<>) as bs)

instance Monoid a => Monoid (Tree a) where
  mempty = Node mempty (repeat mempty)

prop_join t1 t2 t3 = (t1 <> (t2 <> t3)) == ((t1 <> t2) <> t3)

-- quickCheck (withMaxSuccess 10000 prop_join )