module Main where

import LibMain
import Example.Counter
import StateMachine
import Compiler

------------------------------------------------------------------------

main :: IO ()
main = libMain (compile counterSM)
