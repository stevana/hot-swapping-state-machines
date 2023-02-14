module Code where

type Code = [Instruction]

data Instruction
  = ID | FST | SND | COPY | CONSUME | FIRST | SECOND | CONS
  | SWAP | QUOTE Value | ADD | APP | CURRY Code | INL | INR
  | BRANCH Code Code | RETURN | PUSH | ROT | GET | PUT
  deriving (Eq, Show, Read)

data Value = Unit | Pair Value Value | L Value | R Value | Int Int | Closure Code Value
  deriving (Eq, Show, Read)

class IsValue a where
  quote :: a -> Value

instance IsValue () where
  quote () = Unit

instance (IsValue a, IsValue b) => IsValue (a, b) where
  quote (x, y) = Pair (quote x) (quote y)

instance IsValue Int where
  quote = Int

instance (IsValue a, IsValue b) => IsValue (Either a b) where
  quote (Left  x) = L (quote x)
  quote (Right y) = R (quote y)
