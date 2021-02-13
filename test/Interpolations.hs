module Interpolations where

import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import Hedgehog
import qualified Hedgehog.Gen as Gen
import qualified Hedgehog.Range as Range
import Test.Tasty
import Test.Tasty.Hedgehog
import Yasi

test_interpolation :: TestTree
test_interpolation =
  testGroup
    "interpolation"
    [ testProperty "to String" . property $ do
        (a, b, c) <- (,,) <$> forAll genString <*> forAll genText <*> forAll genString
        [i|${a}++$b🙀🙀🙀🙀$c🙀|] === a <> "++" <> T.unpack b <> "🙀🙀🙀🙀" <> c <> "🙀",
      testProperty "to Text" . property $ do
        (a, b, c) <- (,,) <$> forAll genText <*> forAll genText <*> forAll genString
        [i|😶😶$a😶😶$b😶😶$c😶😶|] === "😶😶" <> a <> "😶😶" <> b <> "😶😶" <> T.pack c <> "😶😶",
      testProperty "to ByteString" . property $ do
        (a, b, c) <- (,,) <$> forAll genBS <*> forAll genText <*> forAll genString
        let p = T.encodeUtf8 "℘"
        [i|℘$a℘$b℘$c℘|] === p <> a <> p <> T.encodeUtf8 b <> p <> T.encodeUtf8 (T.pack c) <> p,
      testProperty "abstractions" . property $ do
        (a, b) <- (,) <$> forAll genText <*> forAll genText
        [i|xxx-${}${}-yyy|] a b === "xxx-" <> a <> b <> "-yyy"
    ]
  where
    genString = Gen.string range Gen.unicodeAll
    genText = Gen.text range Gen.unicodeAll
    genBS = Gen.bytes range
    range = Range.constant 0 20
