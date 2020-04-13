module EnvVars.Match where

import Prelude
import Data.Either (Either(..))
import Data.Either as Either
import Data.Int as Int

boolean :: String -> Either String Boolean
boolean = case _ of
  "0" -> Right false
  _ -> Right true

string :: String -> Either String String
string = Right

int :: String -> Either String Int
int = Int.fromString >>> Either.note "Invalid Integer"
