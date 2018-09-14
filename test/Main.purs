module Test.Main where

import Prelude

import Data.List (List, fold, fromFoldable)
import Data.String.CodeUnits (singleton)
import Effect (Effect)
import Parsing.ListParser.Elements (ParseElement(..), element, elements, unParseElement)
import Test.Unit (suite, test)
import Test.Unit.Assert (equal)
import Test.Unit.Main (runTest)
import Text.Parsing.Parser (runParser)
import Text.Parsing.Parser.String (char, string)

-- NOTE: temporary
main :: Effect Unit
main =
  runTest do
    suite "test" do
      test "element" do
        let a = runParser (fold <<< map singleton $ chars) (char 'a')
        let b = map unParseElement $ runParser (map ParseElement chars) (element (ParseElement 'a'))
        equal a b
      test "elements" do
        let a = runParser (fold <<< map singleton $ chars) (string "ab")
        let b = map (fold <<< map (singleton <<< unParseElement)) $ runParser (map ParseElement chars) (elements <<< map ParseElement $ fromFoldable ['a', 'b'])
        equal a b

chars :: List Char
chars = fromFoldable ['a', 'b', 'c']