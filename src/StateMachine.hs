{-# LANGUAGE GADTs #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE StandaloneDeriving #-}

module StateMachine where

import Control.Arrow
import Control.Category (Category)
import qualified Control.Category as Cat

import Code

------------------------------------------------------------------------

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
  Embed   :: (a -> b) -> FreeFunc s a b
  Get     :: FreeFunc s () s
  Put     :: FreeFunc s s ()
  Const   :: (Num a, Show a, IsValue a) => a -> FreeFunc s b a
  Add     :: Num a => FreeFunc s (a, a) a

instance Show (FreeFunc s a b) where
  showsPrec _d Id = showString "Id"
  showsPrec d (Compose g f) = showParen (d > 10) $
    showString "Compose "
      . showsPrec 11 g
      . showString " "
      . showsPrec 11 f
  showsPrec _d Copy = showString "Copy"
  showsPrec _d Consume = showString "Consume"
  showsPrec d (Par f g) = showParen (d > 10) $
    showString "Par "
      . showsPrec 11 f
      . showString " "
      . showsPrec 11 g
  showsPrec _d Fst = showString "Fst"
  showsPrec _d Snd = showString "Snd"
  showsPrec _d Inl = showString "Inl"
  showsPrec _d Inr = showString "Inr"
  showsPrec d (Case f g) = showParen (d > 10) $
    showString "Case "
      . showsPrec 11 f
      . showString " "
      . showsPrec 11 g
  showsPrec d (Embed _) = showParen (d > 10) $
    showString "Embed _"
  showsPrec _d Get = showString "Get"
  showsPrec _d Put = showString "Put"
  showsPrec d (Const k) = showParen (d > 10) $
    showString "Const "
      . showsPrec 11 k
  showsPrec _d Add = showString "Add"

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
