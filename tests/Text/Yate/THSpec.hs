{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE QuasiQuotes #-}

module Text.Yate.THSpec where

import qualified Data.HashMap.Strict as M

import           Test.Hspec

import           Text.Yate.Engine
import           Text.Yate.TH
import           Text.Yate.Types

template :: Template YateValue
template = [ytpl|Hello, {%= name %}!|]

spec :: Spec
spec = describe "Template Haskell" $ do
  it "parses the template properly" $ do
    let dat = Object $ M.fromList [("name", String "Bob")]
    renderTemplate template dat `shouldBe` Right "Hello, Bob!"
