{-# LANGUAGE CPP #-}

module Util where

import qualified Data.Text as T
import Hedgehog
import qualified Hedgehog.Gen as Gen
import qualified Hedgehog.Range as Range
import Test.Tasty
import Test.Tasty.HUnit
import Test.Tasty.Hedgehog
import Yasi.Util

#if !(MIN_VERSION_base(4,11,0))
import Data.Semigroup ((<>))
#endif

test_unindent :: TestTree
test_unindent =
  testGroup
    "unindentation"
    [ testCase "empty string" $ do
        unindent "" @?= "",
      testProperty "single line" . property $ do
        t <- forAll genTextWONL
        unindent t === (T.dropWhile (== ' ') t <> "\n"),
      testCase "multi line" $ do
        unindent "\n  asdf\n\n wtf\n   foo\n" @?= " asdf\n\nwtf\n  foo\n"
    ]
  where
    genTextWONL = Gen.text (Range.constant 1 50) $ Gen.filter (/= '\n') Gen.unicodeAll
