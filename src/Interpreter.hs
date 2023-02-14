{-# LANGUAGE GADTs #-}

module Interpreter where

import StateMachine

------------------------------------------------------------------------

eval1 :: FreeFunc s i o -> (i -> s -> (s, o))
eval1 func i s = case func of
  Id -> (s, i)
  Compose g f -> let (s', o) = eval1 f i s in eval1 g o s'
  Copy -> (s, (i, i))
  Consume -> (s, ())
  Par f g -> let
                 (s',  o)  = eval1 f (fst i) s
                 (s'', o') = eval1 g (snd i) s'
             in
               (s'', (o, o'))
  Fst -> (s, fst i)
  Snd -> (s, snd i)
  Inl -> (s, Left i)
  Inr -> (s, Right i)
  Case f g -> case i of
    Left  l -> eval1 f l s
    Right r -> eval1 g r s
  Embed f -> (s, f i)
  Get -> (s, s)
  Put -> (i, ())
  Const k -> (s, k)
  Add -> (s, uncurry (+) i)

eval :: FreeFunc s i o -> [i] -> s -> (s, [o])
eval f = go []
  where
    go acc []       s = (s, reverse acc)
    go acc (i : is) s =
      let
        (s', o) = eval1 f i s
      in
        go (o : acc) is s'
