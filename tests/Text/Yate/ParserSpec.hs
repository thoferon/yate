module Text.Yate.ParserSpec where

import Control.Applicative

import Data.Attoparsec.Text
import Data.Either

import Test.Hspec

import Text.Yate.Parser.Internal
import Text.Yate.Types

testTemplate :: Template YateValue
testTemplate = Parts
  [ Content "Items owned by "
  , Variable $ AbsolutePath ["user", "name"]
  , Content ":"
  , For "_element" (AbsolutePath ["user", "items"])
        (In (AbsolutePath ["_element"])
            (Parts
               [ Variable $ RelativePath ["description"]
               , Content " "
               , If (RelativePath ["value"])
                    (Parts
                       [ Content "("
                       , Variable $ RelativePath ["value"]
                       , Content ")"
                       ])
                    (Content "")
               ]))
  , Content "End of list."
  ]

spec :: Spec
spec = describe "Parser" $ do
  describe "leftDelimiterParser" $ do
    it "returns the left delimiter as if it was raw content" $ do
      parseOnly (leftDelimiterParser "{%" "%}") "{% left_delimiter %}"
        `shouldBe` Right (Content "{%")

  describe "pathParser" $ do
    it "returns a relative path when it starts with a dot" $ do
      parseOnly pathParser ".one.two"
        `shouldBe` Right (RelativePath ["one", "two"])

    it "returns an absolute path when it starts with a name" $ do
      parseOnly pathParser "one.two"
        `shouldBe` Right (AbsolutePath ["one", "two"])

  describe "variableParser" $ do
    it "returns a '=' statement" $ do
      parseOnly (variableParser "{%" "%}") "{%= x %}"
        `shouldBe` Right (Variable (AbsolutePath ["x"]))

  describe "ifParser" $ do
    it "recurses inside its blocks" $ do
      let content = "{% if some_name %}{% left_delimiter %}{% else %}\
                    \{% left_delimiter %}{% end %}"
          result  =
            If (AbsolutePath ["some_name"]) (Content "{%") (Content "{%")
      parseOnly (ifParser "{%" "%}") content `shouldBe` Right result

    it "sets an empty content for the second block if else is not present" $ do
      let result =
            If (AbsolutePath ["some_name"]) (Content "content") (Content "")
      parseOnly (ifParser "{%" "%}") "{% if some_name %}content{% end %}"
        `shouldBe` Right result

  describe "forParser" $ do
    it "recurses inside its block" $ do
      let content = "{% for x in xs %}{%= x %}{% end %}"
          result  =
            For "x" (AbsolutePath ["xs"]) (Variable (AbsolutePath ["x"]))
      parseOnly (forParser "{%" "%}") content `shouldBe` Right result

  describe "forallParser" $ do
    it "returns an 'in' statement inside a 'for' statement" $ do
      let content = "{% forall users %}{%= .name %}{% end %}"
          result  = For "_element" (AbsolutePath ["users"])
                        (In (AbsolutePath ["_element"])
                            (Variable (RelativePath ["name"])))
      parseOnly (forallParser "{%" "%}") content `shouldBe` Right result

  describe "inParser" $ do
    it "returns an 'in' statement" $ do
      let content = "{% in some.object %}{% left_delimiter %}{% end %}"
          result  = In (AbsolutePath ["some", "object"]) (Content "{%")
      parseOnly (inParser "{%" "%}") content `shouldBe` Right result

  describe "contentParser" $ do
    it "stops on delimiters" $ do
      let parser  = contentParser "{%" "%}" <* leftDelimiterParser "{%" "%}"
          content = "some content{% left_delimiter %}"
          result  = Content "some content"
      parseOnly parser content `shouldBe` Right result

    it "fails if the left delimiter is null" $ do
      parseOnly (contentParser "" ")") "content" `shouldSatisfy` isLeft

  describe "templateParser" $ do
    it "doesn't get confused with things starting like a delimiter" $ do
      let result = Parts
            [Content "\\item{", Variable $ AbsolutePath ["x"], Content "}"]
      parseOnly (templateParser "{%" "%}") "\\item{{%= x %}}"
        `shouldBe` Right result

    it "integrates all the subparsers together" $ do
      let content = "Items owned by {%= user.name %}:\
                    \{% forall user.items %}\
                    \{%= .description %} \
                    \{% if .value %}({%= .value %}){% end %}\
                    \{% end %}\
                    \End of list."
      parseOnly (templateParser "{%" "%}") content `shouldBe` Right testTemplate
