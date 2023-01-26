{-# LANGUAGE GADTs #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE Rank2Types #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE ViewPatterns #-}

module StateMachine where

import Control.Arrow
import Control.Category (Category)
import qualified Control.Category as Cat

------------------------------------------------------------------------

newtype SM s i o = SM { stepSM :: i -> s -> (s, o) }

data FreeFunc s a b where
  Id      :: FreeFunc s a a
  Compose :: FreeFunc s b c -> FreeFunc s a b -> FreeFunc s a c
  Copy    :: FreeFunc s a (a, a)
  Consume :: FreeFunc s a ()
  Par     :: FreeFunc s a b -> FreeFunc s c d -> FreeFunc s (a, c) (b, d)
  Fst     :: FreeFunc s (a, b) a
  Snd     :: FreeFunc s (a, b) b
  Inl     :: FreeFunc s a (Either a b)
  Inr     :: FreeFunc s b (Either a b)
  Case    :: FreeFunc s a c -> FreeFunc s b c -> FreeFunc s (Either a b) c
  Distr   :: FreeFunc s (Either a b, c) (Either (a, c) (b, c))
  App     :: FreeFunc s (a -> b, a) b
  Curry   :: FreeFunc s (a, b) c -> FreeFunc s a (b -> c)
  Embed   :: (a -> b) -> FreeFunc s a b
  Get     :: FreeFunc s () s
  Put     :: FreeFunc s s ()
  Const   :: (Num a, Show a) => a -> FreeFunc s b a
  Add     :: Num a => FreeFunc s (a, a) a

instance Show (FreeFunc s a b) where
  showsPrec d Id = showString "Id"
  showsPrec d (Compose g f) = showParen (d > 10) $
    showString "Compose "
      . showsPrec 11 g
      . showString " "
      . showsPrec 11 f
  showsPrec d Copy = showString "Copy"
  showsPrec d Consume = showString "Consume"
  showsPrec d (Par f g) = showParen (d > 10) $
    showString "Par "
      . showsPrec 11 f
      . showString " "
      . showsPrec 11 g
  showsPrec d Fst = showString "Fst"
  showsPrec d Snd = showString "Snd"
  showsPrec d Inl = showString "Inl"
  showsPrec d Inr = showString "Inr"
  showsPrec d (Case f g) = showParen (d > 10) $
    showString "Case "
      . showsPrec 11 f
      . showString " "
      . showsPrec 11 g
  showsPrec d (Embed _) = showParen (d > 10) $
    showString "Embed _"
  showsPrec d Get = showString "Get"
  showsPrec d Put = showString "Put"
  showsPrec d (Const k) = showParen (d > 10) $
    showString "Const "
      . showsPrec 11 k
  showsPrec d Add = showString "Add"

instance Category (FreeFunc s) where
  id  = Id
  (.) = Compose

instance Arrow (FreeFunc s) where
  arr      = Embed
  first  f = Par f Id
  second g = Par Id g
  (***)    = Par
  (&&&) :: FreeFunc s a b -> FreeFunc s a c -> FreeFunc s a (b, c)
  f &&& g  = Par f g `Compose` Copy

instance ArrowChoice (FreeFunc s) where
  (+++) :: FreeFunc s a c -> FreeFunc s b d -> FreeFunc s (Either a b) (Either c d)
  f +++ g = Case (Inl `Compose` f) (Inr `Compose` g)
  (|||)   = Case
  left  f = Case (Inl `Compose` f) Inr
  right g = Case Inl (Inr `Compose` g)

runSM :: SM s i o -> [i] -> s -> (s, [o])
runSM sm = go []
  where
    go acc []       s = (s, reverse acc)
    go acc (i : is) s =
      let
        (s', o) = stepSM sm i s
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

get :: SM s () s
get = SM (\_i s -> (s, s))

put :: SM s s ()
put = SM (\s' _s -> (s', ()))

------------------------------------------------------------------------

eval1 :: FreeFunc s i o -> (i -> s -> (s, o))
eval1 f i s = case f of
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
  Distr -> case i of
    (Left  x, z) -> (s, Left  (x, z))
    (Right y, z) -> (s, Right (y, z))
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

pattern Tup x y <- (split -> (x, y))
  where Tup x y = pair x y

void :: Port s r a -> Port s r ()
void = encode Consume

box :: (a -> b) -> Port s r a -> Port s r b
box f = encode (Embed f)

konst :: (Num a, Show a) => a -> Port s r () -> Port s r a
konst x = encode (Const x)

pget :: Port s r () -> Port s r s
pget = encode Get

pput :: Port s r s -> Port s r ()
pput = encode Put

pmodify :: FreeFunc s s s -> Port s r () -> Port s r ()
pmodify f = encode (Get >>> f >>> Put)

kadd :: Int -> FreeFunc s Int Int
kadd k = (Const k &&& Id) >>> Add

inl :: Port s r a -> Port s r (Either a b)
inl = encode Inl

inr :: Port s r b -> Port s r (Either a b)
inr = encode Inr

pdistr ::  Port s r (Either a b, c) -> Port s r (Either (a, c) (b, c))
pdistr = encode Distr

kase :: (Port s r a -> Port s r c) -> (Port s r b -> Port s r c) -> Port s r (Either a b) -> Port s r c
kase f g e = undefined

pleft :: Port s x y -> Port s (Either x z) (Either y z)
pleft (P f) = P (left f)

(>>) :: Port s r a -> Port s r b -> Port s r b
x >> y = psnd (pair x y)
