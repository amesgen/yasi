-- | Yet another string interpolator
--
--  * Dead simple
--  * No dependency on [haskell-src-meta](https://hackage.haskell.org/package/haskell-src-meta).
--    It is not actively developed, has long compile times and several bugs, some of which are
--    by design (e.g. operator fixities).
--  * Supports interpolating 'String', 'Data.Text.Text', 'Data.Text.Lazy.Text',
--    'Data.ByteString.ByteString' and 'Data.ByteString.Lazy.ByteString' (UTF8).
module Yasi
  ( i,

    -- * Variants
    iFS,
    iS,
    iT,
    iTL,
    iB,
    iBL,
  )
where

import qualified Data.ByteString as B
import qualified Data.ByteString.Lazy as BL
import Data.String (IsString (..))
import qualified Data.Text as T
import qualified Data.Text.Lazy as TL
import qualified Language.Haskell.TH.Quote as TH
import qualified Language.Haskell.TH.Syntax as TH
import Yasi.Internal

-- $setup
-- >>> import Yasi
-- >>> import Data.Text (Text)
-- >>> import Data.ByteString (ByteString)

int :: (TH.Exp -> TH.Exp) -> TH.QuasiQuoter
int f = interpolator '$' (pure . f)

-- | The main interpolator, intended to be used with
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
i = int id

-- | Like 'i', but works with 'IsString'.
--
-- @['iFS'|...|] = 'fromString' ['i'|...|]@
--
-- >>> :t [iFS|hi|]
-- [iFS|hi|] :: Data.String.IsString a => a
iFS :: TH.QuasiQuoter
iFS = int $ TH.AppE (TH.VarE 'fromString)

intT :: TH.Name -> TH.QuasiQuoter
intT = int . flip TH.SigE . TH.ConT

-- | Like 'i', but with the result type fixed to 'String'.
--
-- @['iS'|...|] = ['i'|...|] :: 'String'@
--
-- >>> :t [iS|hi|]
-- [iS|hi|] :: String
iS :: TH.QuasiQuoter
iS = intT ''String

-- | Like 'i', but with the result type fixed to 'T.Text'.
--
-- @['iT'|...|] = ['i'|...|] :: 'T.Text'@
--
-- >>> :t [iT|hi|]
-- [iT|hi|] :: Text
iT :: TH.QuasiQuoter
iT = intT ''T.Text

-- | Like 'iT', but lazy.
iTL :: TH.QuasiQuoter
iTL = intT ''TL.Text

-- | Like 'i', but with the result type fixed to 'B.ByteString'.
--
-- @['iB'|...|] = ['i'|...|] :: 'B.ByteString'@
--
-- >>> :t [iB|hi|]
-- [iB|hi|] :: ByteString
iB :: TH.QuasiQuoter
iB = intT ''B.ByteString

-- | Like 'iB', but lazy.
iBL :: TH.QuasiQuoter
iBL = intT ''BL.ByteString
