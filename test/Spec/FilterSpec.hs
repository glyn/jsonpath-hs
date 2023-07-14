{-# LANGUAGE OverloadedStrings #-}

module Spec.FilterSpec (spec) where

import Data.JSONPath.Parser(jsonPath)
import Data.Text
import Test.Hspec
import Test.Hspec.Megaparsec
import Text.Megaparsec

filterMissingEndBracketEtoks :: ET Text
filterMissingEndBracketEtoks =
  etoks "!="
    <> etoks "&&"
    <> etoks "<="
    <> etoks "=="
    <> etoks ">="
    <> etoks "||"
    <> etoks "!"
    <> etoks "."
    <> etoks "<"
    <> etoks ">"
    <> etoks "["
    <> etoks "]"

spec :: Spec
spec = do
  describe "filter" $ do
    it "ummatched filter bracket parse error" $
      parse (jsonPath eof) "" "$[?@.foo"
        `shouldFailWith` err 8 (ueof <> filterMissingEndBracketEtoks <> elabel "white space")

