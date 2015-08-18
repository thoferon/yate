module Text.Yate.Parser
  ( parseTemplate
  , parseTemplateWith
  ) where

import           Data.Attoparsec.Text
import qualified Data.Text as T

import           Text.Yate.Parser.Internal
import           Text.Yate.Types

-- | Parse a template and return an error otherwise
parseTemplate :: T.Text -- ^ Content of the template
              -> Either String (Template a)
parseTemplate = parseTemplateWith "{%" "%}"

-- | Same as 'parseTemplate' but take the left and right delimiters
parseTemplateWith :: T.Text -- ^ Left delimiter (default: @{%@)
                  -> T.Text -- ^ Right delimiter (default: @%}@)
                  -> T.Text -- ^ Content of the template
                  -> Either String (Template a)
parseTemplateWith l r = parseOnly $ templateParser l r
