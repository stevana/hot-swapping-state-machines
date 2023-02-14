{-# LANGUAGE GADTs #-}

module Compiler where

import StateMachine
import Code

------------------------------------------------------------------------

compile :: FreeFunc s i o -> Code
compile Id            = [ID]
compile (Compose g f) = compile f ++ compile g
compile Copy          = [COPY]
compile Consume       = [CONSUME]
compile (Par f g)     = FIRST : compile f ++ [SWAP, CONS, SECOND] ++ compile g ++ [CONS]
compile Inl           = [INL]
compile Inr           = [INR]
compile (Case l r)    = [BRANCH (compile l ++ [RETURN]) (compile r ++ [RETURN])]
compile Add           = [ADD]
compile Fst           = [FST]
compile Snd           = [SND]
compile (Const x)     = [QUOTE (quote x)]
compile Get           = [GET]
compile Put           = [PUT]
compile (Embed _f)    = error "compile: should never see `Embed`"
