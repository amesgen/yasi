module Util where

import Data.Text qualified as T
import Hedgehog
import Hedgehog.Gen qualified as Gen
import Hedgehog.Range qualified as Range
import Test.Tasty
import Test.Tasty.HUnit
import Test.Tasty.Hedgehog
import Yasi.Util

test_unindent :: TestTree
test_unindent =
  testGroup
    "unindentation"
    [ testCase "empty string" $ do
        unindent "" @?= "",
      testPropertyNamed "single line" "unindentation_single_line" . property $ do
        t <- forAll genTextWONL
        unindent t === (T.dropWhile (== ' ') t <> "\n"),
      testCase "multi line" $ do
        unindent "\n  asdf\n\n wtf\n   foo\n" @?= " asdf\n\nwtf\n  foo\n"
    ]
  where
    genTextWONL = Gen.text (Range.constant 1 50) $ Gen.filter (/= '\n') Gen.unicodeAll
