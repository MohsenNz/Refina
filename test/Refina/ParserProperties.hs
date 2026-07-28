module Refina.ParserProperties (propertySpec) where

import Relude

import Hedgehog
  ( Property
  , annotate
  , check
  , failure
  , forAll
  , property
  , success
  , (===)
  )
import Refina.AST
import Refina.Gen (genArbitraryText, genExpr)
import Refina.Parser (exprParser, parseModule, parseOnly)
import Test.Hspec (Spec, describe, it, shouldBe)
import Text.Megaparsec (eof)

-- ---------------------------------------------------------------------------
-- Spec wrapper (runs Hedgehog properties inside hspec)
-- ---------------------------------------------------------------------------

propertySpec :: Spec
propertySpec = describe "Parser properties (Hedgehog)" $ do
  it "expression roundtrip: prettyPrint >> parse == id" $ do
    result <- check prop_exprRoundtrip
    result `shouldBe` True
  it "parseModule never crashes on arbitrary input" $ do
    result <- check prop_parseNeverCrashes
    result `shouldBe` True

-- ---------------------------------------------------------------------------
-- Property 1: Expression roundtrip
--   Generate Expr -> pretty-print -> parse -> strip AParens -> compare
-- ---------------------------------------------------------------------------

prop_exprRoundtrip :: Property
prop_exprRoundtrip = property $ do
  expr <- forAll genExpr
  let rendered = prettyExpr 0 expr
  annotate (toString rendered)
  case parseOnly (exprParser <* eof) rendered of
    Left err -> do
      annotate (show err)
      failure
    Right parsed ->
      stripParens parsed === expr

-- ---------------------------------------------------------------------------
-- Property 2: parseModule never throws on arbitrary input
-- ---------------------------------------------------------------------------

prop_parseNeverCrashes :: Property
prop_parseNeverCrashes = property $ do
  input <- forAll genArbitraryText
  case parseModule input of
    Left _ -> success
    Right _ -> success

-- ---------------------------------------------------------------------------
-- Precedence-aware pretty-printer (minimal parenthesization)
--
-- All binary operators are left-associative, so:
--   left child:   parens if child prec < parent prec
--   right child:  parens if child prec <= parent prec
-- ---------------------------------------------------------------------------

prec :: Expr -> Int
prec (EOr _ _) = 1
prec (EAnd _ _) = 2
prec (ECompare _ _ _) = 3
prec (EPipe _ _ _) = 4
prec (ECompose _ _ _) = 5
prec (EAdd _ _ _) = 6
prec (EMul _ _ _) = 7
prec (ENeg _) = 8
prec (EAtom _) = 9

prettyExpr :: Int -> Expr -> Text
prettyExpr minP e
  | prec e < minP = "(" <> go e <> ")"
  | otherwise = go e
  where
    go :: Expr -> Text
    go (EOr l r) = prettyExpr 1 l <> " or " <> prettyExpr 2 r
    go (EAnd l r) = prettyExpr 2 l <> " and " <> prettyExpr 3 r
    go (ECompare l op r) =
      prettyExpr 3 l <> " " <> ppCompareOp op <> " " <> prettyExpr 4 r
    go (EPipe l op r) =
      prettyExpr 4 l <> " " <> ppPipeOp op <> " " <> prettyExpr 5 r
    go (ECompose l op r) =
      prettyExpr 5 l <> " " <> ppComposeOp op <> " " <> prettyExpr 6 r
    go (EAdd l op r) =
      prettyExpr 6 l <> " " <> ppAddOp op <> " " <> prettyExpr 7 r
    go (EMul l op r) =
      prettyExpr 7 l <> " " <> ppMulOp op <> " " <> prettyExpr 8 r
    go (ENeg inner) = "- " <> prettyExpr 8 inner
    go (EAtom a) = ppAtom a

ppAtom :: Atom -> Text
ppAtom APlaceholder = "_"
ppAtom (AIdent (Identifier n)) = n
ppAtom (ALiteral lit) = ppLiteral lit
ppAtom (AParens e) = "(" <> prettyExpr 0 e <> ")"
ppAtom (AFunCall (Identifier f) args) = f <> " " <> unwords (map ppAtom args)
ppAtom (AOpSection _) = error "ppAtom: OpSection not supported in roundtrip"

ppLiteral :: Literal -> Text
ppLiteral (LInt n) = show n
ppLiteral (LBool True) = "true"
ppLiteral (LBool False) = "false"
ppLiteral (LStr s) = "\"" <> s <> "\""

ppCompareOp :: CompareOp -> Text
ppCompareOp CEq = "=="
ppCompareOp CNe = "!="
ppCompareOp CLt = "<"
ppCompareOp CLe = "<="
ppCompareOp CGt = ">"
ppCompareOp CGe = ">="
ppCompareOp CNotMatch = "!~"

ppPipeOp :: PipeOp -> Text
ppPipeOp PipeRight = "|>"
ppPipeOp PipeLeft = "<|"

ppComposeOp :: ComposeOp -> Text
ppComposeOp ComposeRight = ".>"
ppComposeOp ComposeLeft = "<."

ppAddOp :: AddOp -> Text
ppAddOp APlus = "+"
ppAddOp AMinus = "-"

ppMulOp :: MulOp -> Text
ppMulOp MTimes = "*"
ppMulOp MDiv = "/"
ppMulOp MMod = "%"

-- ---------------------------------------------------------------------------
-- Strip AParens wrappers introduced by the parser
--
-- The pretty-printer adds parentheses when a low-precedence subexpression is
-- nested inside a high-precedence one.  The parser wraps those in AParens.
-- Stripping lets us compare the structural expression tree directly.
-- ---------------------------------------------------------------------------

stripParens :: Expr -> Expr
stripParens (EOr l r) = EOr (stripParens l) (stripParens r)
stripParens (EAnd l r) = EAnd (stripParens l) (stripParens r)
stripParens (ECompare l op r) = ECompare (stripParens l) op (stripParens r)
stripParens (EPipe l op r) = EPipe (stripParens l) op (stripParens r)
stripParens (ECompose l op r) = ECompose (stripParens l) op (stripParens r)
stripParens (EAdd l op r) = EAdd (stripParens l) op (stripParens r)
stripParens (EMul l op r) = EMul (stripParens l) op (stripParens r)
stripParens (ENeg e) = ENeg (stripParens e)
stripParens (EAtom (AParens e)) = stripParens e
stripParens (EAtom a) = EAtom a
