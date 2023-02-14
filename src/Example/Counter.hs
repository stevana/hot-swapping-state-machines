{-# LANGUAGE Arrows #-}

module Example.Counter where

import Control.Category (Category)
import qualified Control.Category as Cat
import Control.Arrow

import StateMachine
import Syntax

------------------------------------------------------------------------

data Command = Incr | Read

data Response = Ok | Sum Int
  deriving Show

------------------------------------------------------------------------

newtype SM s i o = SM { stepSM :: i -> s -> (s, o) }

runSM :: SM s i o -> [i] -> s -> (s, [o])
runSM sm0 = go []
  where
    go acc []       s = (s, reverse acc)
    go acc (i : is) s =
      let
        (s', o) = stepSM sm0 i s
      in
        go (o : acc) is s'

instance Category (SM s) where
  id          = SM (\i s -> (s, i))
  SM g . SM f = SM (\i s -> let (s', j) = f i s in g j s')

instance Arrow (SM s) where
  arr f        = SM (\i s -> (s, f i))
  first (SM f) = SM (\(i, k) s -> let (s', o) = f i s in (s', (o, k)))

instance ArrowChoice (SM s) where
  SM f +++ SM g = SM (\e s -> either (\i -> Left <$> f i s) (\j -> Right <$> g j s) e)


counter :: SM Int Command Response
counter = proc i -> case i of
  Incr -> do
    s <- get -< ()
    () <- put -< s + 1
    returnA -< Ok
  Read -> do
    s <- get -< ()
    returnA -< Sum s
  where
    get :: SM s () s
    get = SM (\_i s -> (s, s))

    put :: SM s s ()
    put = SM (\s' _s -> (s', ()))

counter' :: (Arrow a, ArrowChoice a) => a (Command, Int) (Response, Int)
counter' = proc (i, s) -> case i of
  Incr -> returnA -< (Ok, s + 1)
  Read -> returnA -< (Sum s, s)

------------------------------------------------------------------------

counterSM :: FreeFunc Int (Either () ()) (Either () Int)
counterSM = sm $ kase incr get
  where
    incr = inl . pmodify (kadd 1)
    get  = inr . pget

counterSM2 :: FreeFunc Int (Either () ()) (Either () Int)
counterSM2 = sm $ kase incr get
  where
    incr = inl . pmodify (kadd 2)
    get  = inr . pget
