{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE Rank2Types #-}
{-# LANGUAGE ViewPatterns #-}

module Syntax where

import Control.Arrow
import qualified Control.Category as Cat

import StateMachine
import Code

------------------------------------------------------------------------

-- https://acatalepsie.fr/posts/overloading-lambda

newtype Port s r a = P { unPort :: FreeFunc s r a }

encode :: FreeFunc s a b -> Port s r a -> Port s r b
encode f (P x) = P (f Cat.. x)

decode :: (forall r. Port s r a -> Port s r b) -> FreeFunc s a b
decode f = unPort (f (P Cat.id))

pair :: Port s r a -> Port s r b -> Port s r (a, b)
pair (P x) (P y) = P (x &&& y)

unit :: Port s r ()
unit = P Consume

sm :: (forall r. Port s r a -> Port s r b) -> FreeFunc s a b
sm = decode

pfst :: Port s r (a, b) -> Port s r a
pfst = encode Fst

psnd :: Port s r (a, b) -> Port s r b
psnd = encode Snd

split :: Port s r (a, b) -> (Port s r a, Port s r b)
split p = (pfst p, psnd p)

pattern Tup :: Port s r a -> Port s r b -> Port s r (a, b)
pattern Tup x y <- (split -> (x, y))
  where Tup x y = pair x y

void :: Port s r a -> Port s r ()
void = encode Consume

box :: (a -> b) -> Port s r a -> Port s r b
box f = encode (Embed f)

konst :: (Num a, Show a, IsValue a) => a -> Port s r () -> Port s r a
konst x = encode (Const x)

pget :: Port s r () -> Port s r s
pget = encode Get

pput :: Port s r s -> Port s r ()
pput = encode Put

pmodify :: FreeFunc s s s -> Port s r () -> Port s r ()
pmodify f = encode (Get >>> f >>> Put)

kadd :: Int -> FreeFunc s Int Int
kadd k = (Const k &&& Id) >>> Add

padd :: Int -> Port s r Int -> Port s r Int
padd k = encode (kadd k)

inl :: Port s r a -> Port s r (Either a b)
inl = encode Inl

inr :: Port s r b -> Port s r (Either a b)
inr = encode Inr

kase :: (forall q. Port s q a -> Port s q c) -> (forall q. Port s q b -> Port s q c)
     -> Port s r (Either a b) -> Port s r c
kase f g = encode (Case (decode f) (decode g))

pleft :: Port s x y -> Port s (Either x z) (Either y z)
pleft (P f) = P (left f)

(>>) :: Port s r a -> Port s r b -> Port s r b
x >> y = psnd (pair x y)

--
