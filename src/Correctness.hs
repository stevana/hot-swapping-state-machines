module Correctness where

import Code
import StateMachine
import Interpreter
import AbstractMachine

------------------------------------------------------------------------

-- TODO: Generate `FreeFunc a b`...
prop_commute :: (IsValue s, IsValue i, IsValue o) => FreeFunc s i o -> i -> s -> Bool
prop_commute f x s = quote (eval1 f x s) == snd (run f (quote x) (quote s))
