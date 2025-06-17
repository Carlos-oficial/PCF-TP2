module DurationMonad where

import Control.Comonad


-- Defining a monad (the duration monad) --

-- it's a writer monad

-- sames as liftM3 (\x y z-> x + y - z) <$> Just 1 <*> (pure 3) <*> (Just 2)

data Duration a = Duration (Float, a) deriving (Show, Ord, Eq)

getDuration :: Duration a -> Float
getDuration (Duration (d,x)) = d

apDuration :: (Float -> Float) -> Duration a -> Duration a
apDuration f (Duration (d,x)) = Duration (f d, x)

getValue :: Duration a -> a
getValue (Duration (d,x)) = x

instance Functor Duration where
  fmap f (Duration (i,x)) = Duration (i,f x)

instance Applicative Duration where
  pure x = (Duration (0,x))
  (Duration (i,f)) <*> (Duration (j, x)) = (Duration (i+j, f x))
  
instance Monad Duration where
  (Duration (i,x)) >>= k = Duration (i + (getDuration (k x)), getValue (k x))
  return = pure

wait1 :: Duration a -> Duration a
wait1 = wait 1

wait :: Float -> Duration a -> Duration a
wait i (Duration (d,x)) = Duration (i + d, x)

-- Duration CoMonad
instance Comonad Duration where
  extract = getValue
  duplicate = pure
