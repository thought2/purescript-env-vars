module EnvVars
  ( fromRecord, class GEnvVars, gEnvVars, env, parseRecord
  ) where

import Prelude
import Data.Argonaut (Json, decodeJson)
import Data.Either (Either(..))
import Data.Maybe (Maybe(..))
import Data.Tuple.Nested (type (/\), (/\))
import Effect (Effect)
import Foreign.Object (Object)
import Foreign.Object as Object
import Record as Record
import Record.Extra (class SequenceRecord, sequenceRecord)
import SimpleText as SimpleText
import Type.Data.Symbol (class IsSymbol, SProxy(SProxy), reflectSymbol)
import Type.Equality (class TypeEquals)
import Type.Row (class Cons, class Lacks)
import Type.RowList (kind RowList, Nil, Cons, RLProxy(RLProxy), class RowToList)

env ::
  forall a.
  String -> Maybe a -> (String -> Either String a) -> Object String -> Either String a
env name default parse = lookupEnv { name, default, parse }

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
  envJson <- getEnv
  case decodeJson envJson of
    Right envObj -> parseEnvVars envObj # pure
    Left _ -> pure $ Left "Cannot parse environment object"

foreign import getEnv :: Effect Json

class GEnvVars (spec :: # Type) (parsed :: # Type) (specRL :: RowList) | specRL -> parsed spec where
  gEnvVars :: Object String -> Record spec -> RLProxy specRL -> Either String (Record parsed)

instance gEnvVarsNil :: GEnvVars spec () Nil where
  gEnvVars _ _ _ = Right {}

instance gEnvVarsCons ::
  ( GEnvVars spec parsed' specRL
  , IsSymbol sym
  , TypeEquals a' (Maybe a /\ (String -> Either String a))
  , Cons sym (Maybe a /\ (String -> Either String a)) spec' spec
  , Lacks sym spec'
  , Cons sym a parsed' parsed
  , Lacks sym parsed'
  ) =>
  GEnvVars spec parsed (Cons sym a' specRL) where
  gEnvVars obj spec _ =
    let
      sProxy = SProxy :: SProxy sym

      name = reflectSymbol sProxy

      default /\ parse = Record.get sProxy spec

      parseTail = gEnvVars obj spec (RLProxy :: RLProxy specRL)
    in
      do
        tail <- parseTail
        result <- lookupEnv { name, default, parse } obj
        pure $ Record.insert sProxy result tail

fromRecord ::
  forall spec specRL parsed.
  RowToList spec specRL =>
  GEnvVars spec parsed specRL =>
  Record spec -> Object String -> Either String (Record parsed)
fromRecord spec obj = gEnvVars obj spec (RLProxy :: RLProxy specRL)

parseRecord ::
  forall spec specRL' parsed spec' specRL.
  RowToList spec specRL =>
  SequenceRecord specRL spec () spec' (Function (Object String)) =>
  RowToList spec' specRL' =>
  SequenceRecord specRL' spec' () parsed (Either String) =>
  Record spec -> Object String -> Either String (Record parsed)
parseRecord r x = sequenceRecord r # (_ $ x) # sequenceRecord
