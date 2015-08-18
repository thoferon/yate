module Text.Yate.Engine
  ( renderTemplate
  ) where

import           Control.Monad
import           Control.Applicative

import           Data.Monoid
import qualified Data.HashMap.Strict as M
import qualified Data.Text.Lazy      as TL
import qualified Data.Vector         as V

import           Text.Yate.Engine.Internal
import           Text.Yate.Types

renderTemplate :: ToYate a => Template a -> a -> Either String TL.Text
renderTemplate template dat = let val = toYate dat in go template val val
  where
    go :: Template a -> YateValue -> YateValue -> Either String TL.Text
    go tpl main current = case tpl of
      Content  txt   -> return txt
      Variable path  -> showValue <$> resolvePath path main current

      If path yes no -> case valueToBoolean <$> resolvePath path main current of
        Left  _ -> go no main current
        Right b -> go (if b then yes else no) main current

      For name path tpl' -> do
        list <- resolvePath path main current
        case list of
          List values -> do
            let step acc value = do
                  main' <- case main of
                    Object m -> return $ Object $ M.insert name value m
                    _        -> Left "main value is not an object"
                  (acc <>) <$> go tpl' main' current
            V.foldM step "" values
          _ -> Left $ "encountered non-list value on a for statement: "
                      ++ show path

      In path tpl' -> do
        current' <- resolvePath path main current
        go tpl' main current'

      Parts tpls ->
        foldM (\acc tpl' -> (acc <>) <$> go tpl' main current) "" tpls
