module AbstractMachine where

import Code
import StateMachine
import Compiler

------------------------------------------------------------------------

-- [The Categorical Abstract Machine](https://core.ac.uk/download/pdf/82453178.pdf)
-- https://bartoszsypytkowski.com/simple-virtual-machine/

type Stack = [StackElement]

data StackElement = Value Value | Code Code
  deriving Show

type Config = (Value, Code, Value, Stack)

exec1 :: Config -> Either Config Config
exec1 (x,                     ID          :  is, s, st)           = Right (x, is, s, st)
exec1 (Pair l _r,             FST         :  is, s, st)           = Right (l, is, s, st)
exec1 (Pair _l r,             SND         :  is, s, st)           = Right (r, is, s, st)
exec1 (x,                     COPY        :  is, s, st)           = Right (Pair x x, is, s, st)
exec1 (_x,                    CONSUME     :  is, s, st)           = Right (Unit, is, s, st)
exec1 (Pair a x,              FIRST       :  is, s, st)           = Right (a, is, s, Value x : st)
exec1 (Pair x b,              SECOND      :  is, s, st)           = Right (b, is, s, Value x : st)
exec1 (a,                     CONS        :  is, s, Value x : st) = Right (Pair x a, is, s, st)
exec1 (a,                     SWAP        :  is, s, Value b : st) = Right (b, is, s, Value a : st)
exec1 (x,                     PUSH        :  is, s, st)           = Right (x, is, s, Value x : st)
exec1 (x,                     ROT         :  is, s, y : z : st)   = Right (x, is, s, z : y : st)
exec1 (x,                     RETURN      : _is, s, Code c : st)  = Right (x, c, s, st)
exec1 (l,                     INL         :  is, s, st)           = Right (L l, is, s, st)
exec1 (r,                     INR         :  is, s, st)           = Right (R r, is, s, st)
exec1 (L l,                   BRANCH c _d :  is, s, st)           = Right (l, c, s, Code is : st)
exec1 (R r,                   BRANCH _c d :  is, s, st)           = Right (r, d, s, Code is : st)
exec1 (_a,                    QUOTE v     :  is, s, st)           = Right (v, is, s, st)
exec1 (Pair (Int i) (Int j),  ADD         :  is, s, st)           = Right (Int (i + j), is, s, st)
exec1 (Pair (Closure x y) z,  APP         :  is, s, st)           = Right (Pair y z, x, s, Code is : st)
exec1 (x,                     CURRY c     :  is, s, st)           = Right (Closure c x, is, s, st)
exec1 (_x,                    GET         :  is, s, st)           = Right (s, is, s, st)
exec1 (s',                    PUT         :  is, _s, st)          = Right (Unit, is, s', st)
exec1 config                                                      = Left config

exec :: Config -> Config
exec cfg = case exec1 cfg of
  Right cfg' -> exec cfg'
  Left  cfg' -> cfg'

debugExec :: Config -> IO Config
debugExec cfg = case exec1 cfg of
  Right cfg' -> do
    print cfg'
    debugExec cfg'
  Left  cfg' -> return cfg'

run :: FreeFunc s i o -> (Value -> Value -> (Value, Value))
run f i s = (s', o)
  where
    (o, _, s', _) = exec (i, compile f, s, [])

debug :: FreeFunc s a b -> (Value -> Value -> Config)
debug f i s = exec (i, compile f, s, [])
