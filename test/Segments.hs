module Segments where

import Test.Tasty.HUnit
import Yasi.Internal

unit_segment_parsing :: IO ()
unit_segment_parsing = do
  ps "foo\\\"" @?= Just [Lit "foo\\\""]
  ps "$a-$b" @?= Just [Exp "a", Lit "-", Exp "b"]
  ps "asef$" @?= Nothing
  ps "lol$${}" @?= Just [Lit "lol${}"]
  ps "lol$$${}" @?= Just [Lit "lol$", Abs]
  ps "${89+}${}" @?= Just [Exp "89+", Abs]
  ps "${}}" @?= Just [Abs, Lit "}"]
  ps "a$$b" @?= Just [Lit "a$b"]
  ps "$a$b$c" @?= Just [Exp "a", Exp "b", Exp "c"]
  ps "" @?= Just []
  ps " " @?= Just [Lit " "]
  ps "$foobar:\n - $b" @?= Just [Exp "foobar", Lit ":\n - ", Exp "b"]
  ps "${foobar]" @?= Nothing
  ps "test-$0foo" @?= Nothing
  where
    ps = parseSegments '$'
