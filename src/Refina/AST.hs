module Refina.AST
  ( -- * Module-level
    Module (..)
  , SchemaDecl (..)
  , Definition (..)

    -- * Types
  , TypeExpr (..)
  , UnionType (..)
  , TypeTerm (..)
  , PrimitiveType (..)
  , TypeName (..)
  , LiteralType (..)
  , RecordType (..)
  , Field (..)

    -- * Expressions
  , Expr (..)
  , CompareOp (..)
  , PipeOp (..)
  , ComposeOp (..)
  , AddOp (..)
  , MulOp (..)
  , Atom (..)
  , Literal (..)
  , OpSection (..)
  , Identifier (..)
  ) where

import Relude

{- FOURMOLU_DISABLE -}

-- ---------------------------------------------------------------------------
-- Module-level
-- ---------------------------------------------------------------------------

data Module = Module
  { moduleSchema :: Maybe SchemaDecl
  , moduleDefinitions :: [Definition]
  }
  deriving stock (Show, Eq)

data SchemaDecl = SchemaDecl
  { schemaName :: TypeName
  }
  deriving stock (Show, Eq)

data Definition = Definition
  { defName :: TypeName
  , defType :: TypeExpr
  }
  deriving stock (Show, Eq)

-- ---------------------------------------------------------------------------
-- Types
-- ---------------------------------------------------------------------------

-- | A type expression is a union type (the top of the type grammar).
newtype TypeExpr = TypeExpr
  { unTypeExpr :: UnionType
  }
  deriving stock (Show, Eq)

-- | Union type: optional leading @"|"@, then @"|"@-separated terms.
--
-- A single term with no @"|"@ at all is stored as a singleton union.
data UnionType = UnionType
  { unionTerms :: NonEmpty TypeTerm
  }
  deriving stock (Show, Eq)

-- | A term in a union type.
--
-- The @"where"@ refinement is modelled as its own constructor rather than
-- threaded through the whole type tree so that the parser can attach it after
-- parsing any base term.
data TypeTerm
  = TPrimitive !PrimitiveType
  | TLiteral !LiteralType
  | TNamed !TypeName
  | TRecord !RecordType
  | TRefinement !TypeTerm !Expr
  deriving stock (Show, Eq)

-- | Primitive type constructors.
data PrimitiveType
  = TInt
  | TNat
  | TNum
  | TStr
  | TBool
  | TNull
  | TList !TypeExpr
  | TMap !TypeExpr !TypeExpr
  deriving stock (Show, Eq)

-- | A type name must start with an uppercase letter.
newtype TypeName = TypeName
  { unTypeName :: Text
  }
  deriving stock (Show, Eq, Ord)

-- | Singleton literal used as a type.
data LiteralType
  = TLitStr !Text
  | TLitInt !Integer
  | TLitBool !Bool
  deriving stock (Show, Eq)

-- TODO: do this need multi-constructor ?
--       add NonEmpty
-- | A record type is either multi-line or inline.
data RecordType
  = TMultiRecord ![Field]
  | TInlineRecord ![Field]
  deriving stock (Show, Eq)

-- | A single field in a record.
data Field = Field
  { fieldName      :: !Identifier
  , fieldOptional  :: !Bool
  , fieldType      :: !TypeExpr
  }
  deriving stock (Show, Eq)

-- ---------------------------------------------------------------------------
-- Expressions
-- ---------------------------------------------------------------------------

-- | Full expression tree, preserving operator precedence.
data Expr
  = EOr       !Expr !Expr
  | EAnd      !Expr !Expr
  | ECompare  !Expr !CompareOp !Expr
  | EPipe     !Expr !PipeOp !Expr
  | ECompose  !Expr !ComposeOp !Expr
  | EAdd      !Expr !AddOp !Expr
  | EMul      !Expr !MulOp !Expr
  | ENeg      !Expr
  | EAtom     !Atom
  deriving stock (Show, Eq)

data CompareOp = CEq | CNe | CLt | CLe | CGt | CGe | CNotMatch
  deriving stock (Show, Eq)

data PipeOp = PipeRight | PipeLeft
  deriving stock (Show, Eq)

data ComposeOp = ComposeRight | ComposeLeft
  deriving stock (Show, Eq)

data AddOp = APlus | AMinus
  deriving stock (Show, Eq)

data MulOp = MTimes | MDiv | MMod
  deriving stock (Show, Eq)

-- ---------------------------------------------------------------------------
-- Atoms (leaf expressions)
-- ---------------------------------------------------------------------------

data Atom
  = APlaceholder
  | AIdent !Identifier
  | ALiteral !Literal
  | AFunCall !Identifier !(NonEmpty Atom)
  | AOpSection !OpSection
  | AParens !Expr
  deriving stock (Show, Eq)

-- | A literal value.
data Literal
  = LStr !Text
  | LInt !Integer
  | LBool !Bool
  deriving stock (Show, Eq)

-- | Operator sections like @(< 2)@ or @(x +)@.
data OpSection
  = OSCompareLeft  !CompareOp !Expr
  | OSCompareRight !Expr !CompareOp
  | OSAddLeft      !AddOp !Expr
  | OSAddRight     !Expr !AddOp
  | OSMulLeft      !MulOp !Expr
  | OSMulRight     !Expr !MulOp
  deriving stock (Show, Eq)

-- | An identifier must start with a lowercase letter.
newtype Identifier = Identifier
  { unIdent :: Text
  }
  deriving stock (Show, Eq, Ord)
