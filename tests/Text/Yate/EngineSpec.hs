module Text.Yate.EngineSpec where

import           Data.Either
import qualified Data.HashMap.Strict as M
import qualified Data.Vector         as V

import           Test.Hspec

import           Text.Yate.Engine
import           Text.Yate.Engine.Internal
import           Text.Yate.ParserSpec
import           Text.Yate.Types

spec :: Spec
spec = describe "Engine" $ do
  let item1Dat = Object $ M.fromList
        [ ("description", String "Item 1")
        , ("value",       Number 11.99)
        ]
      item2Dat   = Object $ M.fromList [("description", String "Item 2")]
      addressDat = Object $ M.fromList [("city",        String "Mons")]
      userDat    = Object $ M.fromList
        [ ("name",    String "Alice")
        , ("items",   List $ V.fromList [item1Dat, item2Dat])
        , ("address", addressDat)
        ]
      dat = Object $ M.fromList
        [ ("x",     Number 4.2)
        , ("user",  userDat)
        , ("true",  Bool True)
        , ("false", Bool False)
        , ("null",  Null)
        ]

  let xAbsVar               = Variable $ AbsolutePath ["x"]
      trueAbsPath           = AbsolutePath ["true"]
      falseAbsPath          = AbsolutePath ["false"]
      userItemsAbsPath      = AbsolutePath ["user", "items"]
      itemDescriptionAbsVar = Variable $ AbsolutePath ["item", "description"]
      addressAbsPath        = AbsolutePath ["user", "address"]
      cityRelVar            = Variable $ RelativePath ["city"]
      missingAbsPath        = AbsolutePath ["not", "there"]
      betweenBrackets sth   = Parts [Content "(", sth, Content ")"]

  describe "resolvePath" $ do
    context "with an absolute path" $ do
      it "returns the nested YateValue" $ do
        resolvePath (AbsolutePath ["user", "address", "city"]) dat item2Dat
          `shouldBe` Right (String "Mons")

      it "fails when the path is not reachable" $ do
        resolvePath (AbsolutePath ["address", "city"]) dat userDat
          `shouldSatisfy` isLeft

    context "with a relative path" $ do
      it "returns the nested YateValue from the current object" $ do
        resolvePath (RelativePath ["address", "city"]) dat userDat
          `shouldBe` Right (String "Mons")

      it "fails when the path is not reachable" $ do
        resolvePath (RelativePath ["user", "address", "city"]) dat userDat
          `shouldSatisfy` isLeft

  describe "showValue" $ do
    it "shows whole values whenever possible" $ do
      mapM_ (\(val, out) -> showValue val `shouldBe` out)
        [ (String "some string", "some string")
        , (Number 3.14,          "3.14")
        , (Object M.empty,       "[Object]")
        , (List V.empty,         "[List]")
        , (Bool True,            "true")
        , (Bool False,           "false")
        , (Null,                 "null")
        ]

  describe "renderTemplate" $ do
    it "returns raw content untouched" $ do
      renderTemplate (Content "some content") () `shouldBe` Right "some content"

    it "shows variables in the main object" $ do
      renderTemplate (betweenBrackets xAbsVar) dat `shouldBe` Right "(4.2)"

    it "renders the first block of 'if' statements if the condition holds" $ do
      renderTemplate (If trueAbsPath (Content "first") (Content "second")) dat
        `shouldBe` Right "first"

    it "renders the second block of 'if' statements otherwise" $ do
      renderTemplate (If falseAbsPath (Content "first") (Content "second")) dat
        `shouldBe` Right "second"

    it "renders the second block of 'if' statements with missing paths" $ do
      renderTemplate (If missingAbsPath
                         (Content "first") (Content "second")) dat
        `shouldBe` Right "second"

    it "renders the block for each element on a 'for' statement" $ do
      renderTemplate (For "item" userItemsAbsPath itemDescriptionAbsVar) dat
        `shouldBe` Right "Item 1Item 2"

    it "changes the current object for relative paths on 'in' statements" $ do
      renderTemplate cityRelVar dat `shouldSatisfy` isLeft
      renderTemplate (In addressAbsPath cityRelVar) dat
        `shouldBe` Right "Mons"

    it "returns an error if some data is missing" $ do
      renderTemplate xAbsVar () `shouldSatisfy` isLeft

    it "integrates all parts together" $ do
      let result = "Items owned by Alice:\
                   \Item 1 (11.99)\
                   \Item 2 \
                   \End of list."
      renderTemplate testTemplate dat `shouldBe` Right result
