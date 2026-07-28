module Refina.Parser
  ( parseRefina
  , parseModule
  , moduleParser

    -- * Helpers for unit-testing individual parsers
  , parseOnly
  , Parser

    -- * Lexer
  , sc
  , symbol
  , keyword
  , nameChar

    -- * Identifiers
  , typeNameParser
  , identifierParser

    -- * Literals
  , stringLiteralParser
  , integerLiteralParser
  , booleanLiteralParser
  , literalParser
  , literalTypeParser

    -- * Types
  , typeExprParser
  , unionTypeParser
  , typeTermParser
  , baseTypeTermParser
  , primitiveTypeParser
  , literalTypeAsTermParser
  , namedTypeParser
  , recordTypeParser
  , inlineRecordParser
  , multiRecordParser
  , fieldParser

    -- * Expressions
  , exprParser
  , placeholderParser
  , atomParser
  , atomAtomParser
  , functionCallParser
  , operatorSectionParser

    -- * Operator parsers
  , compareOpParser
  , pipeOpParser
  , composeOpParser
  , addOpParser
  , mulOpParser
  ) where

import Data.Char (isSpace)
import Relude hiding (many, some)

import Refina.AST
import Text.Megaparsec
import Text.Megaparsec.Char
import Text.Megaparsec.Char.Lexer qualified as L

-- ---------------------------------------------------------------------------
-- Parser type
-- ---------------------------------------------------------------------------

type Parser = Parsec Void Text

-- ---------------------------------------------------------------------------
-- Public API
-- ---------------------------------------------------------------------------

-- | Parse a complete Refina source file.
parseRefina :: FilePath -> Text -> Either (ParseErrorBundle Text Void) Module
parseRefina = parse moduleParser

-- | Parse a 'Module' from the given text (no file-name context).
parseModule :: Text -> Either (ParseErrorBundle Text Void) Module
parseModule = parse moduleParser ""

-- | Run any parser on a text string (for unit-testing individual parsers).
parseOnly :: Parser a -> Text -> Either (ParseErrorBundle Text Void) a
parseOnly p = parse p ""

-- | The top-level parser for a Refina module.
moduleParser :: Parser Module
moduleParser = do
  sc
  schema <- optional schemaDeclParser
  sc
  defs <- many (try (sc *> definitionParser))
  sc
  eof
  return Module {moduleSchema = schema, moduleDefinitions = defs}

-- ---------------------------------------------------------------------------
-- Lexer helpers
-- ---------------------------------------------------------------------------

-- | Whitespace (including newlines) + comments.
sc :: Parser ()
sc = L.space (void (some (satisfy isSpace) <?> "whitespace")) lineComment empty
  where
    lineComment = L.skipLineComment "--"

-- | Whitespace that does NOT consume newlines (for single-line contexts).
sc' :: Parser ()
sc' =
  L.space
    (void (some (satisfy (\c -> c == ' ' || c == '\t')) <?> "horizontal space"))
    lineComment
    empty
  where
    lineComment = L.skipLineComment "--"

-- | Consume a specific keyword / symbol, then eat spaces.
symbol :: Text -> Parser ()
symbol s = L.symbol sc s >> pure ()

-- | Parse a keyword that must not be followed by identifier chars.
-- Leading whitespace is consumed first so it works after expression terms
-- that leave trailing whitespace unconsumed.
keyword :: Text -> Parser ()
keyword kw = do
  sc
  _ <- string kw
  notFollowedBy nameChar
  sc

-- | Characters allowed in identifiers / type names.
nameChar :: Parser Char
nameChar = letterChar <|> digitChar <|> char '_'

-- | An uppercase identifier →  'TypeName'.
typeNameParser :: Parser TypeName
typeNameParser = do
  c <- upperChar
  rest <- many nameChar
  sc
  pure . TypeName . toText $ c : rest

-- | A lowercase identifier → 'Identifier'.
reservedWords :: [Text]
reservedWords = ["or", "and", "where", "true", "false", "schema"]

identifierParser :: Parser Identifier
identifierParser = do
  c <- lowerChar
  rest <- many nameChar
  sc
  let name = toText $ c : rest
  when (name `elem` reservedWords) $ fail ("reserved keyword: " <> toString name)
  pure $ Identifier name

-- ---------------------------------------------------------------------------
-- String literal (single or double quotes)
-- ---------------------------------------------------------------------------

stringLiteralParser :: Parser Text
stringLiteralParser = do
  q <- char '\'' <|> char '"'
  content <- many (escapedChar q <|> noneOf [q, '\\'])
  void $ char q
  sc
  pure . toText $ content
  where
    escapedChar :: Char -> Parser Char
    escapedChar _ = do
      _ <- char '\\'
      choice
        [ '\\' <$ char '\\'
        , '\n' <$ char 'n'
        , '\t' <$ char 't'
        , '\r' <$ char 'r'
        , '"' <$ char '"'
        , '\'' <$ char '\''
        ]

-- ---------------------------------------------------------------------------
-- Integer literal
-- ---------------------------------------------------------------------------

integerLiteralParser :: Parser Integer
integerLiteralParser = do
  d <- digitChar
  ds <- many digitChar
  sc
  pure
    $ foldl'
      (\acc c -> acc * 10 + toInteger (ord c - ord '0'))
      (toInteger (ord d - ord '0'))
      ds

-- ---------------------------------------------------------------------------
-- Boolean literal
-- ---------------------------------------------------------------------------

booleanLiteralParser :: Parser Bool
booleanLiteralParser =
  try (True <$ keyword "true")
    <|> try (False <$ keyword "false")

-- ---------------------------------------------------------------------------
-- Literal (in expressions)
-- ---------------------------------------------------------------------------

literalParser :: Parser Literal
literalParser =
  LString
    <$> stringLiteralParser
    <|> LBool
    <$> booleanLiteralParser
    <|> LInteger
    <$> integerLiteralParser

-- ---------------------------------------------------------------------------
-- Literal type (in types)
-- ---------------------------------------------------------------------------

literalTypeParser :: Parser LiteralType
literalTypeParser =
  TLitString
    <$> stringLiteralParser
    <|> TLitBool
    <$> booleanLiteralParser
    <|> TLitInteger
    <$> integerLiteralParser

-- ---------------------------------------------------------------------------
-- Schema declaration
-- "schema" TypeName
-- ---------------------------------------------------------------------------

schemaDeclParser :: Parser SchemaDecl
schemaDeclParser = do
  keyword "schema"
  sc
  name <- typeNameParser
  return SchemaDecl {schemaName = name}

-- ---------------------------------------------------------------------------
-- Definition
-- TypeName ":=" TypeExpr
-- ---------------------------------------------------------------------------

definitionParser :: Parser Definition
definitionParser = do
  col <- unPos . sourceColumn <$> getSourcePos
  name <- typeNameParser
  symbol ":="
  ty <- typeExprParser col
  return Definition {defName = name, defType = ty}

-- ---------------------------------------------------------------------------
-- TypeExpr ::= UnionType
-- ---------------------------------------------------------------------------

typeExprParser :: Int -> Parser TypeExpr
typeExprParser refCol = TypeExpr <$> unionTypeParser refCol

-- ---------------------------------------------------------------------------
-- UnionType ::= "|"? TypeTerm ("|" TypeTerm)*
-- ---------------------------------------------------------------------------

unionTypeParser :: Int -> Parser UnionType
unionTypeParser refCol = do
  terms <- unionTermsParser refCol
  pure $ UnionType terms
  where
    unionTermsParser :: Int -> Parser (NonEmpty TypeTerm)
    unionTermsParser refCol' = do
      -- Optional leading "|" (multi-line union with leading pipe)
      mLead <- optional $ try $ do
        _ <- sc *> symbol "|"
        -- After the pipe, we might be on a new line or on the same line
        pure ()

      let parseFirst = case mLead of
            Just _ -> typeTermParser refCol'
            Nothing -> do
              -- Try to parse the first term as usual
              pos <- getOffset
              -- We need to peek: if the next token is "|" but we already consumed it above...
              -- This case is when there's no leading pipe
              try (typeTermParser refCol') <|> do
                -- Maybe the first term starts after a newline with "|"?
                -- In that case we haven't consumed the "|" yet
                _ <- try (sc *> symbol "|")
                typeTermParser refCol'

      first <- parseFirst

      rest <-
        many
          ( do
              _ <- try (sc *> symbol "|")
              typeTermParser refCol'
          )

      pure $ first :| rest

-- ---------------------------------------------------------------------------
-- TypeTerm ::= PrimitiveType | LiteralType | NamedType | RecordType
--           | TypeTerm "where" Expr
-- ---------------------------------------------------------------------------

typeTermParser :: Int -> Parser TypeTerm
typeTermParser refCol = do
  base <- baseTypeTermParser refCol
  -- Try to attach a "where" refinement clause.
  -- Use try because keyword("where") consumes leading whitespace; we need
  -- to backtrack if there is no "where".
  r <-
    optional
      ( try
          ( do
              keyword "where"
              sc
              expr <- exprParser
              pure (TRefinement base expr)
          )
      )
  pure (fromMaybe base r)

baseTypeTermParser :: Int -> Parser TypeTerm
baseTypeTermParser refCol =
  try primitiveTypeParser
    <|> try literalTypeAsTermParser
    <|> try namedTypeParser
    <|> recordTypeParser refCol

-- | Parse a literal as a 'TypeTerm' (singleton literal type).
literalTypeAsTermParser :: Parser TypeTerm
literalTypeAsTermParser = TLiteral <$> literalTypeParser

-- | Parse a named type (just an uppercase identifier).
namedTypeParser :: Parser TypeTerm
namedTypeParser = do
  -- Must be an uppercase identifier, but NOT a keyword / builtin primitive
  sc
  name <- typeNameParser
  let n = unTypeName name
  when (n `elem` ["List", "Map"])
    $ fail (show n <> " requires type arguments")
  pure $ TNamed name

-- ---------------------------------------------------------------------------
-- Primitive types: Int, Nat, Num, Str, Bool, Null, List TypeExpr, Map TypeExpr TypeExpr
-- ---------------------------------------------------------------------------

primitiveTypeParser :: Parser TypeTerm
primitiveTypeParser =
  TPrimitive <$> do
    choice
      [ try (TInt <$ keyword "Int")
      , try (TNat <$ keyword "Nat")
      , try (TNum <$ keyword "Num")
      , try (TStr <$ keyword "Str")
      , try (TBool <$ keyword "Bool")
      , try (TNull <$ keyword "Null")
      , try
          ( do
              keyword "List"
              sc
              -- List takes a single type term, not a full union.
              -- This ensures "List Int | Null" parses as "(List Int) | Null".
              col <- unPos . sourceColumn <$> getSourcePos
              elemTy <- singleTermAsTypeExpr col
              pure $ TList elemTy
          )
      , try
          ( do
              keyword "Map"
              sc
              -- Map takes two single type terms, not full unions.
              col <- unPos . sourceColumn <$> getSourcePos
              keyTy <- singleTermAsTypeExpr col
              valTy <- singleTermAsTypeExpr col
              pure $ TMap keyTy valTy
          )
      ]

-- | Parse a single type term and wrap it as a TypeExpr (singleton union).
-- Used for List/Map arguments to prevent them from greedily consuming unions.
singleTermAsTypeExpr :: Int -> Parser TypeExpr
singleTermAsTypeExpr refCol = do
  term <- typeTermParser refCol
  pure . TypeExpr $ UnionType (term :| [])

-- ---------------------------------------------------------------------------
-- Record types
-- RecordType ::= MultiRecord | InlineRecord
-- ---------------------------------------------------------------------------

recordTypeParser :: Int -> Parser TypeTerm
recordTypeParser refCol = TRecord <$> (try inlineRecordParser <|> multiRecordParser refCol)

-- | InlineRecord ::= InlineField ("," InlineField)* ";"
-- All on one line (or at least delimited by ",")
inlineRecordParser :: Parser RecordType
inlineRecordParser = do
  fields <- sepBy1 fieldParser (symbol ",")
  sc
  void $ char ';'
  pure $ TInlineRecord fields

-- | MultiRecord ::= Field+ ";"
-- Fields are indented relative to the parent column position.
multiRecordParser :: Int -> Parser RecordType
multiRecordParser refCol = do
  fields <- some (indentFieldParser refCol)
  -- closing ";" (may be on a new line)
  sc
  void $ char ';'
  pure $ TMultiRecord fields

-- | Parse one field in a multi-record, requiring column >= refCol.
indentFieldParser :: Int -> Parser Field
indentFieldParser refCol = try $ do
  sc
  col <- unPos . sourceColumn <$> getSourcePos
  when (col < refCol) $ fail "field is not sufficiently indented"
  fieldParser

-- | Field ::= Identifier Optional? ":" TypeExpr
fieldParser :: Parser Field
fieldParser = do
  name <- identifierParser
  opt <- isJust <$> optional (try (symbol "?"))
  symbol ":"
  col <- unPos . sourceColumn <$> getSourcePos
  ty <- typeExprParser col
  return Field {fieldName = name, fieldOptional = opt, fieldType = ty}

-- ---------------------------------------------------------------------------
-- Expressions
-- ---------------------------------------------------------------------------

exprParser :: Parser Expr
exprParser = orParser

orParser :: Parser Expr
orParser = do
  left <- andParser
  rest <- many (try (keyword "or" *> andParser))
  pure $ foldl' EOr left rest

andParser :: Parser Expr
andParser = do
  left <- compareParser
  rest <- many (try (keyword "and" *> compareParser))
  pure $ foldl' EAnd left rest

compareParser :: Parser Expr
compareParser = do
  left <- pipeParser
  rest <-
    many
      ( try
          ( do
              op <- compareOpParser
              right <- pipeParser
              pure (op, right)
          )
      )
  pure $ foldl' (\l (op, r) -> ECompare l op r) left rest

pipeParser :: Parser Expr
pipeParser = do
  left <- composeParser
  rest <-
    many
      ( try
          ( do
              op <- pipeOpParser
              right <- composeParser
              pure (op, right)
          )
      )
  pure $ foldl' (\l (op, r) -> EPipe l op r) left rest

composeParser :: Parser Expr
composeParser = do
  left <- addParser
  rest <-
    many
      ( try
          ( do
              op <- composeOpParser
              right <- addParser
              pure (op, right)
          )
      )
  pure $ foldl' (\l (op, r) -> ECompose l op r) left rest

addParser :: Parser Expr
addParser = do
  left <- mulParser
  rest <-
    many
      ( try
          ( do
              op <- addOpParser
              right <- mulParser
              pure (op, right)
          )
      )
  pure $ foldl' (\l (op, r) -> EAdd l op r) left rest

mulParser :: Parser Expr
mulParser = do
  left <- unaryParser
  rest <-
    many
      ( try
          ( do
              op <- mulOpParser
              right <- unaryParser
              pure (op, right)
          )
      )
  pure $ foldl' (\l (op, r) -> EMul l op r) left rest

unaryParser :: Parser Expr
unaryParser =
  try
    ( do
        sc
        _ <- symbol "-"
        e <- unaryParser
        pure $ ENeg e
    )
    <|> atomParser

-- ---------------------------------------------------------------------------
-- Operators
-- ---------------------------------------------------------------------------

compareOpParser :: Parser CompareOp
compareOpParser =
  sc
    *> choice
      [ CNotMatch <$ try (string "!~") <* sc
      , CLe <$ try (string "<=") <* sc
      , CGe <$ try (string ">=") <* sc
      , CNe <$ try (string "!=") <* sc
      , CEq <$ try (string "==") <* sc
      , CLt <$ char '<' <* sc
      , CGt <$ char '>' <* sc
      ]

pipeOpParser :: Parser PipeOp
pipeOpParser =
  sc
    *> choice
      [ PipeRight <$ try (string "|>") <* sc
      , PipeLeft <$ try (string "<|") <* sc
      ]

composeOpParser :: Parser ComposeOp
composeOpParser =
  sc
    *> choice
      [ ComposeRight <$ try (string ".>") <* sc
      , ComposeLeft <$ try (string "<.") <* sc
      ]

addOpParser :: Parser AddOp
addOpParser =
  sc
    *> choice
      [ APlus <$ char '+' <* sc
      , AMinus <$ char '-' <* sc
      ]

mulOpParser :: Parser MulOp
mulOpParser =
  sc
    *> choice
      [ MTimes <$ char '*' <* sc
      , MDiv <$ char '/' <* sc
      , MMod <$ char '%' <* sc
      ]

-- ---------------------------------------------------------------------------
-- Atoms
-- ---------------------------------------------------------------------------

atomParser :: Parser Expr
atomParser =
  EAtom
    <$> choice
      [ try operatorSectionParser
      , try functionCallParser
      , try parensParser
      , try placeholderParser
      , try (do sc; i <- identifierParser; pure $ AIdent i)
      , try (do sc; l <- literalParser; pure $ ALiteral l)
      ]
  where
    parensParser = do
      _ <- symbol "("
      e <- exprParser
      _ <- symbol ")"
      pure $ AParens e

placeholderParser :: Parser Atom
placeholderParser = do
  sc
  void $ char '_'
  pure APlaceholder

-- | FunctionCall ::= Identifier Atom+
functionCallParser :: Parser Atom
functionCallParser = do
  sc
  fn <- identifierParser
  args <- some atomAtomParser
  pure $ AFunCall fn args

-- | Parse an atomic expression (same as atomic parts of Atom but we need to
--   avoid infinite recursion — the atoms that can be arguments to a function call).
atomAtomParser :: Parser Atom
atomAtomParser =
  choice
    [ try operatorSectionParser
    , try parensAtomParser
    , try placeholderParser
    , try (do sc; i <- identifierParser; pure $ AIdent i)
    , try (do sc; l <- literalParser; pure $ ALiteral l)
    ]
  where
    parensAtomParser = do
      _ <- symbol "("
      e <- exprParser
      _ <- symbol ")"
      pure $ AParens e

-- ---------------------------------------------------------------------------
-- Operator sections
-- OperatorSection ::= "(" CompareOp Expr ")" | "(" Expr CompareOp ")"
--                   | "(" "+" Expr ")" | "(" Expr "+" ")"
--                   | "(" "-" Expr ")" | "(" Expr "-" ")"
--                   | "(" "*" Expr ")" | "(" Expr "*" ")"
--                   | "(" "/" Expr ")" | "(" Expr "/" ")"
--                   | "(" "%" Expr ")" | "(" Expr "%" ")"
-- ---------------------------------------------------------------------------

operatorSectionParser :: Parser Atom
operatorSectionParser = do
  _ <- symbol "("
  -- Try all section forms:
  --  right section:   (op expr)   e.g. (< 2)
  --  left section:    (expr op)   e.g. (x <)
  opSectionBody <-
    try opSectionCompareRight
      <|> try opSectionCompareLeft
      <|> try opSectionAddRight
      <|> try opSectionAddLeft
      <|> try opSectionMulRight
      <|> try opSectionMulLeft
  _ <- symbol ")"
  pure $ AOpSection opSectionBody

opSectionCompareRight :: Parser OpSection
opSectionCompareRight = do
  op <- compareOpParser
  e <- exprParser
  pure $ OSCompareLeft op e

opSectionCompareLeft :: Parser OpSection
opSectionCompareLeft = do
  e <- exprParser
  op <- compareOpParser
  pure $ OSCompareRight e op

opSectionAddRight :: Parser OpSection
opSectionAddRight = do
  -- Note: '-' in right-section position is parsed as unary negation (see docs §16),
  -- so only '+' is valid here.
  sc
  void $ char '+'
  sc
  e <- exprParser
  pure $ OSAddLeft APlus e

opSectionAddLeft :: Parser OpSection
opSectionAddLeft = do
  e <- exprParser
  op <- addOpParser
  pure $ OSAddRight e op

opSectionMulRight :: Parser OpSection
opSectionMulRight = do
  op <- mulOpParser
  e <- exprParser
  pure $ OSMulLeft op e

opSectionMulLeft :: Parser OpSection
opSectionMulLeft = do
  e <- exprParser
  op <- mulOpParser
  pure $ OSMulRight e op
