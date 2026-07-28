module Refina.Gen
  ( genExpr
  , genAtom
  , genLiteral
  , genIdentifier
  , genCompareOp
  , genPipeOp
  , genComposeOp
  , genAddOp
  , genMulOp
  , genArbitraryText
  ) where

import Relude hiding (many, some)

import Hedgehog
import Hedgehog.Gen qualified as Gen
import Hedgehog.Range qualified as Range
import Refina.AST

-- ---------------------------------------------------------------------------
-- Expression generator (recursive, with shrinking via subterm)
-- ---------------------------------------------------------------------------

genExpr :: Gen Expr
genExpr =
  Gen.recursive
    Gen.choice
    [ EAtom <$> genAtom
    ]
    [ Gen.subterm2 genExpr genExpr EOr
    , Gen.subterm2 genExpr genExpr EAnd
    , do
        op <- genCompareOp
        Gen.subterm2 genExpr genExpr (\l r -> ECompare l op r)
    , do
        op <- genPipeOp
        Gen.subterm2 genExpr genExpr (\l r -> EPipe l op r)
    , do
        op <- genComposeOp
        Gen.subterm2 genExpr genExpr (\l r -> ECompose l op r)
    , do
        op <- genAddOp
        Gen.subterm2 genExpr genExpr (\l r -> EAdd l op r)
    , do
        op <- genMulOp
        Gen.subterm2 genExpr genExpr (\l r -> EMul l op r)
    , Gen.subterm genExpr ENeg
    ]

-- ---------------------------------------------------------------------------
-- Atom generator (non-recursive leaves only)
-- ---------------------------------------------------------------------------

genAtom :: Gen Atom
genAtom =
  Gen.choice
    [ pure APlaceholder
    , AIdent <$> genIdentifier
    , ALiteral <$> genLiteral
    ]

genLiteral :: Gen Literal
genLiteral =
  Gen.choice
    [ LInt <$> Gen.integral (Range.linear 0 9999)
    ]

genIdentifier :: Gen Identifier
genIdentifier =
  Gen.element
    [ Identifier "a"
    , Identifier "b"
    , Identifier "x"
    , Identifier "y"
    , Identifier "foo"
    , Identifier "bar"
    , Identifier "name"
    , Identifier "val"
    ]

-- ---------------------------------------------------------------------------
-- Operator generators
-- ---------------------------------------------------------------------------

genCompareOp :: Gen CompareOp
genCompareOp = Gen.element [CEq, CNe, CLt, CLe, CGt, CGe, CNotMatch]

genPipeOp :: Gen PipeOp
genPipeOp = Gen.element [PipeRight, PipeLeft]

genComposeOp :: Gen ComposeOp
genComposeOp = Gen.element [ComposeRight, ComposeLeft]

genAddOp :: Gen AddOp
genAddOp = Gen.element [APlus, AMinus]

genMulOp :: Gen MulOp
genMulOp = Gen.element [MTimes, MDiv, MMod]

-- ---------------------------------------------------------------------------
-- Arbitrary text for robustness testing
-- ---------------------------------------------------------------------------

genArbitraryText :: Gen Text
genArbitraryText =
  Gen.text (Range.linear 0 120) genRefinaChar

genRefinaChar :: Gen Char
genRefinaChar =
  Gen.frequency
    [ (5, Gen.lower)
    , (3, Gen.upper)
    , (2, Gen.digit)
    , (3, Gen.element [' ', '\n', '\t'])
    , (4, Gen.element [':', ';', ',', '(', ')', '_', '?', '"', '\''])
    , (3, Gen.element ['|', '<', '>', '=', '!', '~', '.', '+', '-', '*', '/', '%'])
    ]
