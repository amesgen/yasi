module Segments where

import Test.Tasty.HUnit
import Yasi.Internal

unit_segment_parsing :: IO ()
unit_segment_parsing = do
  ps "foo\\\"" @?= Just [Lit "foo\\\""]
  ps "$a-$b" @?= Just [Var "a", Lit "-", Var "b"]
  ps "asef$" @?= Nothing
  ps "lol$${}" @?= Just [Lit "lol${}"]
  ps "lol$$${}" @?= Just [Lit "lol$", Abs]
  ps "${89+}${}" @?= Just [Var "89+", Abs]
  ps "${}}" @?= Just [Abs, Lit "}"]
  ps "asdf${show bar}$$" @?= Just [Lit "asdf", ShowVar "bar", Lit "$"]
  ps "a$$b" @?= Just [Lit "a$b"]
  ps "$a$b$c" @?= Just [Var "a", Var "b", Var "c"]
  ps "" @?= Just []
  ps " " @?= Just [Lit " "]
  ps "$foobar:\n - $b" @?= Just [Var "foobar", Lit ":\n - ", Var "b"]
  ps "${foobar]" @?= Nothing
  ps "test-$0foo" @?= Nothing
  ps "${show }" @?= Just [ShowVar ""]
  ps "${show}" @?= Just [ShowAbs]
  ps "$show" @?= Just [ShowAbs]
  where
    ps = parseSegments '$'
