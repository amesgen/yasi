-- | Internal module, no stability guarantees
module Yasi.Internal
  ( Segment (..),
    parseSegments,
    ipExpr,
    interpolator,
    Stringish (..),
  )
where

import Control.Monad ((>=>))
import Data.Char qualified as C
import Data.List (foldl')
import Data.Text qualified as T
import Data.Text.Builder.Linear qualified as TBL
import Data.Text.Display qualified as TD
import Data.Text.Lazy qualified as TL
import GHC.Generics (Generic)
import Language.Haskell.Meta.Parse qualified as GhcHsMeta
import Language.Haskell.TH.Lib qualified as TH
import Language.Haskell.TH.Quote qualified as TH
import Language.Haskell.TH.Syntax qualified as TH

data Segment
  = Lit String
  | Exp String
  | Abs -- idea due to interpolate
  deriving (Show, Eq, Generic)

parseSegments :: (MonadFail m) => Char -> String -> m [Segment]
parseSegments c = fmap (group []) . go
  where
    -- ugly, but simple enough™
    go s
      | let (lit, rest) = span (/= c) s, not (null lit) = (Lit lit :) <$> go rest
      | s == "" = pure []
      | s == [c] = fail $ "should not end with single " <> [c]
      | _ : c' : rest <- s, c == c' = (Lit [c] :) <$> go rest
      | _ : '{' : rest <- s = case span (/= '}') rest of -- TODO smarter?
          (exp, '}' : rest) ->
            let seg = if exp == "" then Abs else Exp exp
             in (seg :) <$> go rest
          _ -> fail "missing closing bracket"
      | _ : v : rest' <- s,
        isVarStartChar v =
          let (vs, rest) = span isVarChar rest'
           in (Exp (v : vs) :) <$> go rest
      | otherwise = fail $ "invalid char after " <> [c]
    isVarStartChar v = C.isAscii v && C.isAlpha v
    isVarChar v = C.isAscii v && (C.isAlphaNum v || v == '_' || v == '\'')

    group ls [] = lit ls
    group ls (Lit l : ss) = group (l : ls) ss
    group ls (s : ss) = lit ls <> (s : group [] ss)
    lit [] = []
    lit ls = [Lit $ mconcat $ reverse ls]

ipExpr :: (TH.Exp -> TH.Exp) -> [Segment] -> TH.Q TH.Exp
ipExpr transform segs = do
  (ls, lams) <- go segs
  pure
    . lams
    . transform
    . TH.AppE (TH.VarE 'stringish)
    . foldl' (\a b -> TH.InfixE (Just a) (TH.VarE '(<>)) (Just b)) (TH.VarE 'mempty)
    $ ls
  where
    go = \case
      [] -> pure ([], id)
      s : ss -> prep (go ss) case s of
        Lit l -> (,id) <$> [|$(TH.stringE l) :: String|]
        Exp e -> do
          exts <- TH.extsEnabled
          exp <- case GhcHsMeta.parseExpWithExts exts e of
            Right e -> pure e
            Left (line, col, msg) ->
              fail . unlines $
                [ "Parse error at splice `" <> e <> "`:",
                  show line <> ":" <> show col <> ": " <> msg
                ]
          pure (exp, id)
        Abs -> do
          n <- TH.newName "int"
          pure (TH.VarE n, TH.LamE [TH.VarP n])
    prep asg af = do
      (as, g) <- asg
      (a, f) <- af
      pure (TH.AppE (TH.VarE 'TD.displayBuilder) a : as, f . g)

interpolator ::
  Char ->
  -- | postprocess the 'TH.Exp'
  (TH.Exp -> TH.Exp) ->
  TH.QuasiQuoter
interpolator c pp = TH.QuasiQuoter {..}
  where
    quoteExp = parseSegments c >=> ipExpr pp
    quotePat = const $ fail "pattern context not supported"
    quoteType = const $ fail "type context not supported"
    quoteDec = const $ fail "declaration context not supported"

class Stringish a where
  stringish :: TBL.Builder -> a

instance Stringish String where
  stringish = T.unpack . TBL.runBuilder

instance Stringish T.Text where
  stringish = TBL.runBuilder

instance Stringish TL.Text where
  stringish = TL.fromStrict . TBL.runBuilder
