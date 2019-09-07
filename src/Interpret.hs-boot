module Interpret (
    Value(..),
) where

import Parser (Label)

data Value = VVariable Label | VPi Label Value Value | VLambda Label Value | VApp Value Value | VBlank | VInt Int | TInt | TType | VPrimitive String (Value -> Maybe Value)

instance Show Value
