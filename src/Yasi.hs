-- | Yet another string interpolator
--
--  * Very simple, few dependencies.
--  * Based on 'Data.Text.Display.Display' instead of 'Show'.
--  * Depends on [ghc-hs-meta](https://hackage.haskell.org/package/ghc-hs-meta)
--    instead of [haskell-src-meta](https://hackage.haskell.org/package/haskell-src-meta)
--    for interpolating arbitrary expressions.
--    This results in faster compile times and fewer bugs.
module Yasi
  ( i,

    -- * Variants
    iFS,
    iS,
    iT,
    iTL,
  )
where

import Data.String (IsString (..))
import Data.Text qualified as T
import Data.Text.Lazy qualified as TL
import Language.Haskell.TH.Quote qualified as TH
import Language.Haskell.TH.Syntax qualified as TH
import Yasi.Internal

-- $setup
-- >>> import Yasi
-- >>> import Data.Text (Text)
-- >>> import qualified Data.Text.Lazy
-- >>> import Data.String (IsString)

int :: (TH.Exp -> TH.Exp) -> TH.QuasiQuoter
int = interpolator '$'

intT :: TH.Name -> TH.QuasiQuoter
intT = int . flip TH.SigE . TH.ConT

-- | The main interpolator, intended to be used with
-- [@QuasiQuotes@](https://downloads.haskell.org/~ghc/latest/docs/html/users_guide/exts/template_haskell.html#extension-QuasiQuotes).
--
-- >>> :set -XQuasiQuotes
--
-- >>> (foo, bar) = ("yet another ", "interpolator")
-- >>> [i|${foo}string $bar|] :: String
-- "yet another string interpolator"
--
-- The result type can be 'String', strict 'T.Text' or lazy 'TL.Text'.
--
-- You can also use @${}@ to create a function interpolator (this "abstraction" feature is inspired by
-- [interpolate](https://hackage.haskell.org/package/interpolate)):
--
-- >>> [i|more ${}${} code${replicate 3 '!'}|] "point" "free" :: Text
-- "more pointfree code!!!"
i :: TH.QuasiQuoter
i = int id

-- | Like 'i', but works with 'IsString'.
--
-- @['iFS'|...|] = 'fromString' ['i'|...|]@
--
-- >>> :t [iFS|hi|]
-- [iFS|hi|] :: IsString a => a
iFS :: TH.QuasiQuoter
iFS = int $ TH.AppE (TH.VarE 'fromString)

-- | Like 'i', but with the result type fixed to 'String'.
--
-- @['iS'|...|] = ['i'|...|] :: 'String'@
--
-- >>> :t [iS|hi|]
-- [iS|hi|] :: String
iS :: TH.QuasiQuoter
iS = intT ''String

-- | Like 'i', but with the result type fixed to strict 'T.Text'.
--
-- @['iT'|...|] = ['i'|...|] :: 'T.Text'@
--
-- >>> :t [iT|hi|]
-- [iT|hi|] :: Text
iT :: TH.QuasiQuoter
iT = intT ''T.Text

-- | Like 'i', but with the result type fixed to lazy 'TL.Text'.
--
-- @['iTL'|...|] = ['i'|...|] :: 'TL.Text'@
--
-- >>> :t [iTL|hi|]
-- [iTL|hi|] :: Data.Text.Lazy.Text
iTL :: TH.QuasiQuoter
iTL = intT ''TL.Text
