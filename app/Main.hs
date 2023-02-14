module Main where

import LibMain
import Example.Counter
import StateMachine

------------------------------------------------------------------------

main :: IO ()
main = libMain (compile counterSM)
