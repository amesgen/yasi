module Interpolations where

import Control.Applicative (Const (..))
import Data.Text qualified as T
import Data.Text.Display (display)
import Data.Text.Lazy qualified as TL
import Hedgehog
import Hedgehog.Gen qualified as Gen
import Hedgehog.Range qualified as Range
import Test.Tasty
import Test.Tasty.HUnit
import Test.Tasty.Hedgehog
import Yasi

test_interpolation :: TestTree
test_interpolation =
  testGroup
    "interpolation"
    [ testPropertyNamed "to String" "interpolation_to_String" . property $ do
        (a, b, c) <- (,,) <$> forAll genString <*> forAll genText <*> forAll genString
        [i|${a}++$b🙀🙀🙀🙀$c🙀|] === a <> "++" <> T.unpack b <> "🙀🙀🙀🙀" <> c <> "🙀",
      testPropertyNamed "to Text" "interpolation_to_Text" . property $ do
        (a, b, c) <- (,,) <$> forAll genText <*> forAll genText <*> forAll genString
        [i|😶😶$a😶😶$b😶😶$c😶😶|] === "😶😶" <> a <> "😶😶" <> b <> "😶😶" <> T.pack c <> "😶😶",
      testPropertyNamed "abstractions" "interpolation_abstractions" . property $ do
        (a, b :: Int) <- (,) <$> forAll genText <*> forAll genInt
        [i|xxx-${}${}-yyy|] a b === "xxx-" <> a <> display b <> "-yyy",
      testCase "interpolationg expressions" do
        let foo :: Int -> Int -> Int
            foo a b = a + b - 3
        [iT|${1+1 :: Int}-${foo 2 4}|] @?= T.pack "2-3"
        [iTL|${1+1 :: Int}-${foo 2 4}|] @?= TL.pack "2-3"
        [iFS|${1+1 :: Int}-${foo 2 4}|] @?= (Const "2-3" :: Const String (Int -> Bool))
    ]
  where
    genString = Gen.string range Gen.unicode
    genText = Gen.text range Gen.unicode
    range = Range.constant 0 20
    genInt = Gen.integral $ Range.constant 0 1000
