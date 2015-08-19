module Text.Yate
  ( Template
  , YateValue(..)
  , ToYate(..)
  , parseTemplate
  , renderTemplate
  , renderTextTemplate
  ) where

import qualified Data.Text      as T
import qualified Data.Text.Lazy as TL

import           Text.Yate.Engine
import           Text.Yate.Parser
import           Text.Yate.Types

-- | Parse and render a template in one function call
renderTextTemplate :: ToYate a => T.Text -> a -> Either String TL.Text
renderTextTemplate content dat = do
  tpl <- parseTemplate content
  renderTemplate tpl $ toYate dat
