{-# LANGUAGE CPP #-}

-- | Internal module, no stability guarantees
module Yasi.Internal
  ( Segment (..),
    parseSegments,
    ipExpr,
    interpolator,
    Stringy (..),
  )
where

import Control.Monad ((>=>))
import qualified Data.ByteString as B
import qualified Data.ByteString.Lazy as BL
import qualified Data.Char as C
import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import qualified Data.Text.Lazy as TL
import qualified Data.Text.Lazy.Encoding as TL
import GHC.Generics (Generic)
import qualified Language.Haskell.TH.Quote as TH
import qualified Language.Haskell.TH.Syntax as TH
#if !(MIN_VERSION_base(4,13,0))
import Control.Monad.Fail (MonadFail)
#endif
#if !(MIN_VERSION_base(4,11,0))
import Data.Semigroup ((<>))
#endif

data Segment
  = Lit String
  | Var String
  | ShowVar String
  | Abs -- idea due to interpolate
  | ShowAbs
  deriving (Show, Eq, Generic)

parseSegments :: MonadFail m => Char -> String -> m [Segment]
parseSegments c = fmap (group []) . go
  where
    -- rewrite this
    go s
      | let (lit, rest) = span (/= c) s, not (null lit) = (Lit lit :) <$> go rest
      | s == "" = pure []
      | s == [c] = fail $ "should not end with single " <> [c]
      | _ : c' : rest <- s, c == c' = (Lit [c] :) <$> go rest
      | _ : '{' : rest <- s = case span (/= '}') rest of
        (var, '}' : rest) ->
          let seg = case var of
                "" -> Abs
                "show" -> ShowAbs
                's' : 'h' : 'o' : 'w' : ' ' : var -> ShowVar var
                var -> Var var
           in (seg :) <$> go rest
        _ -> fail "missing closing bracket"
      | _ : v : rest' <- s,
        isVarStartChar v =
        let (vs, rest) = span isVarChar rest'
            var = v : vs
            s = if var == "show" then ShowAbs else Var var
         in (s :) <$> go rest
      | otherwise = fail $ "invalid char after " <> [c]
    isVarStartChar v = C.isAscii v && C.isAlpha v
    isVarChar v = C.isAscii v && (C.isAlphaNum v || v == '_' || v == '\'')

    group ls [] = lit ls
    group ls (Lit l : ss) = group (l : ls) ss
    group ls (s : ss) = lit ls ++ (s : group [] ss)
    lit [] = []
    lit ls = [Lit $ mconcat $ reverse ls]

ipExpr :: TH.Exp -> (TH.Exp -> TH.Exp) -> [Segment] -> TH.Q TH.Exp
ipExpr cast combine segs = do
  (ls, lams) <- go segs
  pure $ lams $ combine $ TH.ListE ls
  where
    go = \case
      [] -> pure ([], id)
      (s : ss) -> prep (go ss) $ case s of
        Lit l -> pure (TH.SigE (TH.LitE (TH.StringL l)) (TH.ConT ''String), id)
        Var v -> pure (TH.VarE (TH.mkName v), id)
        ShowVar v -> pure (TH.AppE (TH.VarE 'show) $ TH.VarE (TH.mkName v), id)
        Abs -> do
          n <- TH.newName "int"
          pure (TH.VarE n, TH.LamE [TH.VarP n])
        ShowAbs -> do
          n <- TH.newName "showint"
          pure (TH.AppE (TH.VarE 'show) $ TH.VarE n, TH.LamE [TH.VarP n])
    prep asg af = do
      (as, g) <- asg
      (a, f) <- af
      pure (TH.AppE cast a : as, f . g)

interpolator ::
  Char ->
  -- | postprocess the 'TH.Exp'
  (TH.Exp -> TH.Exp) ->
  TH.QuasiQuoter
interpolator c pp = TH.QuasiQuoter {..}
  where
    quoteExp = parseSegments c >=> ipExpr (TH.VarE 'stringy) (pp . TH.AppE (TH.VarE 'mconcat))
    quotePat = const $ fail "pattern context not supported"
    quoteType = const $ fail "type context not supported"
    quoteDec = const $ fail "declaration context not supported"

class Stringy a b where
  stringy :: a -> b

instance Stringy String String where
  stringy = id

instance Stringy String T.Text where
  stringy = T.pack

instance Stringy String TL.Text where
  stringy = TL.pack

instance Stringy String B.ByteString where
  stringy = T.encodeUtf8 . T.pack

instance Stringy String BL.ByteString where
  stringy = TL.encodeUtf8 . TL.pack

instance Stringy T.Text T.Text where
  stringy = id

instance Stringy T.Text String where
  stringy = T.unpack

instance Stringy T.Text TL.Text where
  stringy = TL.fromStrict

instance Stringy T.Text B.ByteString where
  stringy = T.encodeUtf8

instance Stringy T.Text BL.ByteString where
  stringy = BL.fromStrict . T.encodeUtf8

instance Stringy TL.Text TL.Text where
  stringy = id

instance Stringy TL.Text String where
  stringy = TL.unpack

instance Stringy TL.Text T.Text where
  stringy = TL.toStrict

instance Stringy TL.Text B.ByteString where
  stringy = BL.toStrict . TL.encodeUtf8

instance Stringy TL.Text BL.ByteString where
  stringy = TL.encodeUtf8

instance Stringy B.ByteString B.ByteString where
  stringy = id

instance Stringy B.ByteString BL.ByteString where
  stringy = BL.fromStrict

instance Stringy BL.ByteString BL.ByteString where
  stringy = id

instance Stringy BL.ByteString B.ByteString where
  stringy = BL.toStrict
