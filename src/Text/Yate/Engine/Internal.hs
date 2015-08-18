module Text.Yate.Engine.Internal
  ( resolvePath
  , showValue
  , valueToBoolean
  ) where

import qualified Data.Text           as T
import qualified Data.Text.Lazy      as TL
import qualified Data.HashMap.Strict as M

import           Text.Yate.Types

resolvePath :: Path -> YateValue -> YateValue -> Either String YateValue
resolvePath path main current = case path of
    AbsolutePath names -> go names main
    RelativePath names -> go names current

  where
    go :: [T.Text] -> YateValue -> Either String YateValue
    go [] value = return value
    go (name : names) (Object m) = case M.lookup name m of
      Nothing     -> Left $ "path is missing: " ++ show path
      Just value' -> go names value'
    go _ _ =
      Left $ "encountered non-object value when resolving path: " ++ show path

showValue :: YateValue -> TL.Text
showValue value = case value of
  String txt -> TL.fromChunks [txt]
  Number num -> TL.pack $ show num
  Object _   -> "[Object]"
  List   _   -> "[List]"
  Bool True  -> "true"
  Bool False -> "false"
  Null       -> "null"

-- | Return 'True' for everything except for null and booleans themselves
valueToBoolean :: YateValue -> Bool
valueToBoolean value = case value of
  String _ -> True
  Number _ -> True
  Object _ -> True
  List   _ -> True
  Bool   b -> b
  Null     -> False
