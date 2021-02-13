-- | Yet another string interpolator
--
--  * Dead simple
--  * No dependency on [haskell-src-meta](https://hackage.haskell.org/package/haskell-src-meta).
--    It is not actively developed, has long compile times and several bugs, some of which are
--    by design (e.g. operator fixities).
--  * Supports interpolating 'String', 'Data.Text.Text', 'Data.Text.Lazy.Text',
--    'Data.ByteString.ByteString' and 'Data.ByteString.Lazy.ByteString' (UTF8).
module Yasi (i) where

import qualified Language.Haskell.TH.Quote as TH
import Yasi.Internal

-- $setup
-- >>> import Yasi
-- >>> import Data.Text (Text)
-- >>> import Data.ByteString (ByteString)

-- | The interpolator, intended to be used with
-- [@QuasiQuotes@](https://downloads.haskell.org/~ghc/latest/docs/html/users_guide/exts/template_haskell.html#extension-QuasiQuotes).
--
-- >>> :set -XQuasiQuotes
--
-- >>> (foo, bar) = ("yet another ", "interpolator")
-- >>> [i|${foo}string $bar|] :: String
-- "yet another string interpolator"
--
-- You can also use @${}@ to create a function interpolator (this "abstraction" feature is inspired by
-- [interpolate](https://hackage.haskell.org/package/interpolate)):
--
-- >>> [i|more ${}${} code|] "point" "free" :: Text
-- "more pointfree code"
--
-- To use 'show' to interpolate a value:
--
-- >>> let x = 1 + 1 in [i|1 + 1 = ${show x}|] :: ByteString
-- "1 + 1 = 2"
-- >>> [i|2 + 2 = $show|] (2 + 2) :: Text
-- "2 + 2 = 4"
i :: TH.QuasiQuoter
i = interpolator '$' pure pure
