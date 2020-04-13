module EnvVars where

import Prelude
import Data.Either (Either(..))
import Data.Maybe (Maybe(..))
import Effect (Effect)
import Foreign.Object (Object)
import Foreign.Object as Object
import Node.Process as Process
import SimpleText as SimpleText

lookupEnv ::
  forall a.
  { name :: String
  , default :: Maybe a
  , parse :: String -> Either String a
  } ->
  Object String -> Either String a
lookupEnv { name, default, parse } obj = case Object.lookup name obj, default of
  Just value, _ -> parse value
  Nothing, Just default' -> Right default'
  _, _ ->
    Left
      ( SimpleText.print
          $ SimpleText.Sentence
          $ SimpleText.Texts
              [ SimpleText.Text "Expected environment variable"
              , SimpleText.Backtick $ SimpleText.Text name
              ]
      )

getEnvVars :: forall a. (Object String -> Either String a) -> Effect (Either String a)
getEnvVars parseEnvVars = do
  envObj <- Process.getEnv
  parseEnvVars envObj # pure
