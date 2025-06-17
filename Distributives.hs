module Distributive where

f :: (a -> [b]) -> a -> [a -> b]
f gen = g
  where
    g a = 
      let bs = gen a 
      in
        [\a -> b | b <- bs ]
