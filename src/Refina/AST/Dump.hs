module Refina.AST.Dump (prettyModule) where

import Data.List.NonEmpty qualified as NE
import Prettyprinter
import Refina.AST
import Relude

-- ---------------------------------------------------------------------------
-- AST tree dump (multi-line with indentation)
-- ---------------------------------------------------------------------------

prettyModule :: Module -> Doc ()
prettyModule = dumpModule

dumpModule :: Module -> Doc ()
dumpModule (Module mschema defs) =
  "Module"
    <> line
    <> indent
      2
      ( vcat
          [ "schema:" <+> maybe "Nothing" dumpSchemaDecl mschema
          , "definitions:" <+> pretty (length defs)
              <> line
              <> indent
                2
                ( vcat
                    [ viaShow i <> ":" <+> dumpDefinition d
                    | (i, d) <- zip [0 ..] defs
                    ]
                )
          ]
      )

dumpSchemaDecl :: SchemaDecl -> Doc ()
dumpSchemaDecl (SchemaDecl (TypeName n)) =
  "SchemaDecl"
    <> line
    <> indent
      2
      ("schemaName:" <+> dquotes (pretty n))

dumpDefinition :: Definition -> Doc ()
dumpDefinition (Definition (TypeName n) ty) =
  "Definition"
    <> line
    <> indent
      2
      ( vcat
          [ "defName:" <+> dquotes (pretty n)
          , "defType:" <> line <> indent 2 (dumpTypeExpr ty)
          ]
      )

dumpTypeExpr :: TypeExpr -> Doc ()
dumpTypeExpr (TypeExpr u) =
  "TypeExpr" <> line <> indent 2 (dumpUnionType u)

dumpUnionType :: UnionType -> Doc ()
dumpUnionType (UnionType terms) =
  "UnionType"
    <> line
    <> indent
      2
      ( vcat
          [ "unionTerms:" <+> pretty (NE.length terms)
              <> line
              <> indent
                2
                ( vcat
                    [ viaShow i <> ":" <+> dumpTypeTerm t
                    | (i, t) <- zip [0 ..] (toList terms)
                    ]
                )
          ]
      )

dumpTypeTerm :: TypeTerm -> Doc ()
dumpTypeTerm = \case
  TPrimitive p -> "TPrimitive" <+> dumpPrimitive p
  TLiteral lt -> "TLiteral" <+> dumpLiteralType lt
  TNamed (TypeName n) -> "TNamed" <+> dquotes (pretty n)
  TRecord r -> "TRecord" <> line <> indent 2 (dumpRecordType r)
  TRefinement base expr ->
    "TRefinement"
      <> line
      <> indent
        2
        ( vcat
            [ "base:" <> line <> indent 2 (dumpTypeTerm base)
            , "expr:" <> line <> indent 2 (dumpExpr expr)
            ]
        )

dumpPrimitive :: PrimitiveType -> Doc ()
dumpPrimitive = \case
  TInt -> "TInt"
  TNat -> "TNat"
  TNum -> "TNum"
  TStr -> "TStr"
  TBool -> "TBool"
  TNull -> "TNull"
  TList ty -> "TList" <> line <> indent 2 (dumpTypeExpr ty)
  TMap k v ->
    "TMap"
      <> line
      <> indent
        2
        ( vcat
            [ "key:" <> line <> indent 2 (dumpTypeExpr k)
            , "val:" <> line <> indent 2 (dumpTypeExpr v)
            ]
        )

dumpLiteralType :: LiteralType -> Doc ()
dumpLiteralType = \case
  TLitStr s -> "TLitStr" <+> dquotes (pretty s)
  TLitInt n -> "TLitInt" <+> pretty n
  TLitBool b -> "TLitBool" <+> bool "False" "True" b

dumpRecordType :: RecordType -> Doc ()
dumpRecordType = \case
  TMultiRecord fs -> "TMultiRecord" <> line <> indent 2 (dumpFields fs)
  TInlineRecord fs -> "TInlineRecord" <> line <> indent 2 (dumpFields fs)

dumpFields :: [Field] -> Doc ()
dumpFields fs =
  "fields:" <+> pretty (length fs)
    <> line
    <> indent
      2
      ( vcat
          [ viaShow i <> ":" <+> dumpField f
          | (i, f) <- zip [0 ..] fs
          ]
      )

dumpField :: Field -> Doc ()
dumpField (Field (Identifier n) opt ty) =
  "Field"
    <> line
    <> indent
      2
      ( vcat
          [ "fieldName:" <+> dquotes (pretty n)
          , "fieldOptional:" <+> bool "False" "True" opt
          , "fieldType:" <> line <> indent 2 (dumpTypeExpr ty)
          ]
      )

-- Expression dumping
dumpExpr :: Expr -> Doc ()
dumpExpr = \case
  EOr l r ->
    "EOr"
      <> line
      <> indent
        2
        ( vcat
            ["left:" <> line <> indent 2 (dumpExpr l), "right:" <> line <> indent 2 (dumpExpr r)]
        )
  EAnd l r ->
    "EAnd"
      <> line
      <> indent
        2
        ( vcat
            ["left:" <> line <> indent 2 (dumpExpr l), "right:" <> line <> indent 2 (dumpExpr r)]
        )
  ECompare l op r ->
    "ECompare"
      <> line
      <> indent
        2
        ( vcat
            [ "left:" <> line <> indent 2 (dumpExpr l)
            , "op:" <+> dumpCompareOp op
            , "right:" <> line <> indent 2 (dumpExpr r)
            ]
        )
  EPipe l op r ->
    "EPipe"
      <> line
      <> indent
        2
        ( vcat
            [ "left:" <> line <> indent 2 (dumpExpr l)
            , "op:" <+> dumpPipeOp op
            , "right:" <> line <> indent 2 (dumpExpr r)
            ]
        )
  ECompose l op r ->
    "ECompose"
      <> line
      <> indent
        2
        ( vcat
            [ "left:" <> line <> indent 2 (dumpExpr l)
            , "op:" <+> dumpComposeOp op
            , "right:" <> line <> indent 2 (dumpExpr r)
            ]
        )
  EAdd l op r ->
    "EAdd"
      <> line
      <> indent
        2
        ( vcat
            [ "left:" <> line <> indent 2 (dumpExpr l)
            , "op:" <+> dumpAddOp op
            , "right:" <> line <> indent 2 (dumpExpr r)
            ]
        )
  EMul l op r ->
    "EMul"
      <> line
      <> indent
        2
        ( vcat
            [ "left:" <> line <> indent 2 (dumpExpr l)
            , "op:" <+> dumpMulOp op
            , "right:" <> line <> indent 2 (dumpExpr r)
            ]
        )
  ENeg x -> "ENeg" <> line <> indent 2 (dumpExpr x)
  EAtom a -> "EAtom" <+> dumpAtom a

dumpCompareOp :: CompareOp -> Doc ()
dumpCompareOp = \case
  CEq -> "CEq"
  CNe -> "CNe"
  CLt -> "CLt"
  CLe -> "CLe"
  CGt -> "CGt"
  CGe -> "CGe"
  CNotMatch -> "CNotMatch"

dumpPipeOp :: PipeOp -> Doc ()
dumpPipeOp = \case
  PipeRight -> "PipeRight"
  PipeLeft -> "PipeLeft"

dumpComposeOp :: ComposeOp -> Doc ()
dumpComposeOp = \case
  ComposeRight -> "ComposeRight"
  ComposeLeft -> "ComposeLeft"

dumpAddOp :: AddOp -> Doc ()
dumpAddOp = \case
  APlus -> "APlus"
  AMinus -> "AMinus"

dumpMulOp :: MulOp -> Doc ()
dumpMulOp = \case
  MTimes -> "MTimes"
  MDiv -> "MDiv"
  MMod -> "MMod"

dumpAtom :: Atom -> Doc ()
dumpAtom = \case
  APlaceholder -> "APlaceholder"
  AIdent (Identifier n) -> "AIdent" <+> dquotes (pretty n)
  ALiteral lit -> "ALiteral" <+> dumpLiteral lit
  AFunCall (Identifier f) args ->
    "AFunCall"
      <> line
      <> indent
        2
        ( vcat
            [ "fn:" <+> dquotes (pretty f)
            , "args:" <+> pretty (NE.length args)
                <> line
                <> indent
                  2
                  (vcat [dumpAtom a | a <- toList args])
            ]
        )
  AOpSection os -> "AOpSection" <+> dumpOpSection os
  AParens e -> "AParens" <> line <> indent 2 (dumpExpr e)

dumpLiteral :: Literal -> Doc ()
dumpLiteral = \case
  LStr s -> "LStr" <+> dquotes (pretty s)
  LInt n -> "LInt" <+> pretty n
  LBool b -> "LBool" <+> bool "False" "True" b

dumpOpSection :: OpSection -> Doc ()
dumpOpSection = \case
  OSCompareLeft op e ->
    "OSCompareLeft"
      <> line
      <> indent
        2
        (vcat ["op:" <+> dumpCompareOp op, "expr:" <> line <> indent 2 (dumpExpr e)])
  OSCompareRight e op ->
    "OSCompareRight"
      <> line
      <> indent
        2
        (vcat ["expr:" <> line <> indent 2 (dumpExpr e), "op:" <+> dumpCompareOp op])
  OSAddLeft op e ->
    "OSAddLeft"
      <> line
      <> indent 2 (vcat ["op:" <+> dumpAddOp op, "expr:" <> line <> indent 2 (dumpExpr e)])
  OSAddRight e op ->
    "OSAddRight"
      <> line
      <> indent 2 (vcat ["expr:" <> line <> indent 2 (dumpExpr e), "op:" <+> dumpAddOp op])
  OSMulLeft op e ->
    "OSMulLeft"
      <> line
      <> indent 2 (vcat ["op:" <+> dumpMulOp op, "expr:" <> line <> indent 2 (dumpExpr e)])
  OSMulRight e op ->
    "OSMulRight"
      <> line
      <> indent 2 (vcat ["expr:" <> line <> indent 2 (dumpExpr e), "op:" <+> dumpMulOp op])
