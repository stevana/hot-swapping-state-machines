{-# LANGUAGE Arrows #-}
{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE QualifiedDo #-}

module Example.Counter where

import Control.Category
import Control.Arrow

import StateMachine

------------------------------------------------------------------------

data Command = Incr | Read

data Response = Ok | Sum Int
  deriving Show

counter :: SM Int Command Response
counter = proc i -> case i of
  Incr -> do
    s <- get -< ()
    () <- put -< s + 1
    returnA -< Ok
  Read -> do
    s <- get -< ()
    returnA -< Sum s

counter' :: (Arrow a, ArrowChoice a) => a (Command, Int) (Response, Int)
counter' = proc (i, s) -> case i of
  Incr -> returnA -< (Ok, s + 1)
  Read -> returnA -< (Sum s, s)

counterSM :: FreeFunc Int (Either () ()) (Either () Int)
counterSM = sm $ \i -> kase i incr read
  where
    incr = inl (pmodify (kadd 1) unit)
    read = inr (pget unit)

t :: FreeFunc s (a, b) (b, a)
t = sm \(Tup x y) -> Tup y x
