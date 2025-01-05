-- | Utilities for working with (multi-line) literals.
module Yasi.Util
  ( unindent,
  )
where

import Data.Char qualified as C
import Data.List (dropWhileEnd)
import Data.Text qualified as T

-- $setup
-- >>> import Yasi
-- >>> import qualified Data.Text.IO as T

-- | "Unindent" a string.
--
-- Also removes leading and trailing blank lines.
--
-- >>> :{
-- txt = unindent [iT|
--     foo
--       bar
--     baz
-- |]
-- :}
--
-- >>> T.putStr txt
-- foo
--   bar
-- baz
unindent :: T.Text -> T.Text
unindent txt = T.unlines . map (T.drop ind) $ ls
  where
    isWS = T.all C.isSpace
    ls = dropWhile isWS . dropWhileEnd isWS . T.lines $ txt
    ind = case T.length . T.takeWhile (== ' ') <$> filter (not . isWS) ls of
      [] -> 0
      inds -> minimum inds
