{-# LANGUAGE TemplateHaskell #-}

module Text.Yate.TH
  ( ytpl
  , ytplf
  , loadTemplate
  , loadTemplateFile
  ) where

import qualified Data.Text      as T
import qualified Data.Text.Lazy as TL

import           Language.Haskell.TH
import           Language.Haskell.TH.Quote

import           Text.Yate.Parser
import           Text.Yate.Types

ytpl :: QuasiQuoter
ytpl = QuasiQuoter
  { quoteExp  = loadTemplate
  , quotePat  = error "The tpl quasiquoter is only for expressions"
  , quoteType = error "The tpl quasiquoter is only for expressions"
  , quoteDec  = error "The tpl quasiquoter is only for expressions"
  }

ytplf :: QuasiQuoter
ytplf = quoteFile ytpl

loadTemplate :: String -> Q Exp
loadTemplate str = case parseTemplate $ T.pack str of
  Left  err -> fail $ "Failed to parse template: " ++ err
  -- We don't care about the type argument here, it won't stay anyway
  Right tpl -> templateToExp (tpl :: Template ())

loadTemplateFile :: FilePath -> Q Exp
loadTemplateFile path = do
  content <- runIO $ readFile path
  loadTemplate content

templateToExp :: Template a -> Q Exp
templateToExp tpl = case tpl of
  Content  txt  -> [| Content $(litE $ StringL $ TL.unpack txt) |]
  Variable path -> [| Variable $(pathToExp path) |]
  If path yes no ->
    [| If $(pathToExp path) $(templateToExp yes) $(templateToExp no) |]
  For name path block ->
    [| For $(litE $ StringL $ T.unpack name) $(pathToExp path)
           $(templateToExp block) |]
  In path block -> [| In $(pathToExp path) $(templateToExp block) |]
  Parts parts -> [| Parts $(listE $ map templateToExp parts) |]

pathToExp :: Path -> Q Exp
pathToExp path = case path of
    AbsolutePath names -> [| AbsolutePath $(mkNameList names) |]
    RelativePath names -> [| RelativePath $(mkNameList names) |]
  where
    mkNameList :: [T.Text] -> Q Exp
    mkNameList names = listE $ map (litE . StringL . T.unpack) names
