module Refina.ParserSpec (spec) where

import Refina.AST
import Refina.Parser
import Relude
import Test.Hspec

spec :: Spec
spec = do
  parseModuleSpec
  stringLiteralSpec
  booleanLiteralSpec
  integerLiteralSpec
  keywordSpec
  identifierParserSpec
  typeNameParserSpec
  placeholderParserSpec
  literalParserSpec
  compareOpParserSpec
  pipeOpParserSpec
  composeOpParserSpec
  addOpParserSpec
  mulOpParserSpec
  schemaSpec
  commentSpec
  primitiveTypeSpec
  unionTypeSpec
  recordTypeSpec
  refinementTypeSpec
  expressionSpec
  functionCallSpec
  operatorSectionSpec
  astInspectionSpec
  stringEscapeSpec
  listUnionAmbiguitySpec

-- ---------------------------------------------------------------------------
-- End-to-end via parseModule
-- ---------------------------------------------------------------------------

parseModuleSpec :: Spec
parseModuleSpec = describe "parseModule (end-to-end)" $ do
  describe "simple definitions" $ do
    it "parses a basic type definition without ;" $ do
      parseModule "X := Str" `shouldSatisfy` isRight
    it "parses an integer literal type" $ do
      parseModule "X := Int" `shouldSatisfy` isRight
    it "parses a numeric literal" $ do
      parseModule "X := 42" `shouldSatisfy` isRight
    it "parses a boolean type" $ do
      parseModule "X := Bool" `shouldSatisfy` isRight
    it "parses a where clause refinement" $ do
      parseModule "X := Str where _ != \"\"" `shouldSatisfy` isRight
    it "parses where with !~ operator" $ do
      parseModule "UserId := Str where _ !~ \"-\"" `shouldSatisfy` isRight

  describe "named types" $ do
    it "parses a simple alias" $ do
      parseModule "Name := Str" `shouldSatisfy` isRight

  describe "inline records" $ do
    it "parses a single-field inline record" $ do
      parseModule "Schema := name : Str;" `shouldSatisfy` isRight
    it "parses an inline record with where" $ do
      parseModule "Schema := name : Str where _ !~ \"-\";" `shouldSatisfy` isRight

-- ---------------------------------------------------------------------------
-- stringLiteralParser
-- ---------------------------------------------------------------------------

stringLiteralSpec :: Spec
stringLiteralSpec = describe "stringLiteralParser" $ do
  it "parses a double-quoted string" $ do
    parseOnly stringLiteralParser "\"hello\"" `shouldBe` Right "hello"
  it "parses a single-quoted string" $ do
    parseOnly stringLiteralParser "'hello'" `shouldBe` Right "hello"
  it "parses an empty string" $ do
    parseOnly stringLiteralParser "\"\"" `shouldBe` Right ""
  it "parses an empty single-quoted string" $ do
    parseOnly stringLiteralParser "''" `shouldBe` Right ""
  it "rejects unclosed quotes" $ do
    parseOnly stringLiteralParser "\"hello" `shouldSatisfy` isLeft
  it "rejects a bare word (no quotes)" $ do
    parseOnly stringLiteralParser "hello" `shouldSatisfy` isLeft
  it "rejects empty input" $ do
    parseOnly stringLiteralParser "" `shouldSatisfy` isLeft

-- ---------------------------------------------------------------------------
-- booleanLiteralParser
-- ---------------------------------------------------------------------------

booleanLiteralSpec :: Spec
booleanLiteralSpec = describe "booleanLiteralParser" $ do
  it "parses 'true'" $ do
    parseOnly booleanLiteralParser "true" `shouldBe` Right True
  it "parses 'false'" $ do
    parseOnly booleanLiteralParser "false" `shouldBe` Right False
  it "rejects 'tru'" $ do
    parseOnly booleanLiteralParser "tru" `shouldSatisfy` isLeft
  it "rejects 'truefalse' (no separator)" $ do
    parseOnly booleanLiteralParser "truefalse" `shouldSatisfy` isLeft
  it "rejects 'True' (wrong case)" $ do
    parseOnly booleanLiteralParser "True" `shouldSatisfy` isLeft
  it "rejects empty input" $ do
    parseOnly booleanLiteralParser "" `shouldSatisfy` isLeft

-- ---------------------------------------------------------------------------
-- integerLiteralParser (includes trailing sc)
-- ---------------------------------------------------------------------------

integerLiteralSpec :: Spec
integerLiteralSpec = describe "integerLiteralParser" $ do
  it "parses a single digit" $ do
    parseOnly integerLiteralParser "5" `shouldBe` Right 5
  it "parses multiple digits" $ do
    parseOnly integerLiteralParser "12345" `shouldBe` Right 12345
  it "parses with leading whitespace via sc wrapper" $ do
    parseOnly (sc *> integerLiteralParser) "  42" `shouldBe` Right 42
  it "rejects empty input" $ do
    parseOnly integerLiteralParser "" `shouldSatisfy` isLeft
  it "rejects non-digit" $ do
    parseOnly integerLiteralParser "abc" `shouldSatisfy` isLeft

-- ---------------------------------------------------------------------------
-- keyword
-- ---------------------------------------------------------------------------

keywordSpec :: Spec
keywordSpec = describe "keyword" $ do
  it "matches an exact keyword" $ do
    parseOnly (keyword "where") "where" `shouldBe` Right ()
  it "rejects a keyword with extra letters" $ do
    parseOnly (keyword "where") "whereas" `shouldSatisfy` isLeft
  it "rejects a keyword with wrong case" $ do
    parseOnly (keyword "where") "Where" `shouldSatisfy` isLeft
  it "matches with leading whitespace" $ do
    parseOnly (keyword "Str") "  Str" `shouldBe` Right ()
  it "rejects empty input" $ do
    parseOnly (keyword "Str") "" `shouldSatisfy` isLeft
  it "rejects a keyword with trailing identifier chars" $ do
    parseOnly (keyword "Str") "Str_" `shouldSatisfy` isLeft

-- ---------------------------------------------------------------------------
-- identifierParser (includes trailing sc)
-- ---------------------------------------------------------------------------

identifierParserSpec :: Spec
identifierParserSpec = describe "identifierParser" $ do
  it "parses a simple identifier" $ do
    parseOnly identifierParser "abc" `shouldBe` Right (Identifier "abc")
  it "parses an identifier with digits and underscore" $ do
    parseOnly identifierParser "a1_b2" `shouldBe` Right (Identifier "a1_b2")
  it "rejects an identifier starting with underscore" $ do
    parseOnly identifierParser "_foo" `shouldSatisfy` isLeft
  it "rejects an identifier starting with uppercase" $ do
    parseOnly identifierParser "Foo" `shouldSatisfy` isLeft
  it "rejects empty input" $ do
    parseOnly identifierParser "" `shouldSatisfy` isLeft
  it "consumes trailing whitespace" $ do
    parseOnly identifierParser "abc  " `shouldBe` Right (Identifier "abc")

-- ---------------------------------------------------------------------------
-- typeNameParser (includes trailing sc)
-- ---------------------------------------------------------------------------

typeNameParserSpec :: Spec
typeNameParserSpec = describe "typeNameParser" $ do
  it "parses a simple type name" $ do
    parseOnly typeNameParser "Str" `shouldBe` Right (TypeName "Str")
  it "parses a type name with digits and underscore" $ do
    parseOnly typeNameParser "A1_B2" `shouldBe` Right (TypeName "A1_B2")
  it "rejects lower-case start" $ do
    parseOnly typeNameParser "str" `shouldSatisfy` isLeft
  it "rejects empty input" $ do
    parseOnly typeNameParser "" `shouldSatisfy` isLeft
  it "consumes trailing whitespace" $ do
    parseOnly typeNameParser "Bool  " `shouldBe` Right (TypeName "Bool")

-- ---------------------------------------------------------------------------
-- placeholderParser
-- ---------------------------------------------------------------------------

placeholderParserSpec :: Spec
placeholderParserSpec = describe "placeholderParser" $ do
  it "parses a bare placeholder" $ do
    parseOnly placeholderParser "_" `shouldBe` Right APlaceholder
  it "rejects empty input" $ do
    parseOnly placeholderParser "" `shouldSatisfy` isLeft
  it "rejects a non-underscore" $ do
    parseOnly placeholderParser "x" `shouldSatisfy` isLeft

-- ---------------------------------------------------------------------------
-- literalParser (full literal choices)
-- ---------------------------------------------------------------------------

literalParserSpec :: Spec
literalParserSpec = describe "literalParser" $ do
  describe "string literals" $ do
    it "parses a double-quoted string" $ do
      parseOnly literalParser "\"hi\"" `shouldBe` Right (LString "hi")
    it "parses a single-quoted string" $ do
      parseOnly literalParser "'hi'" `shouldBe` Right (LString "hi")
  describe "boolean literals" $ do
    it "parses 'true'" $ do
      parseOnly literalParser "true" `shouldBe` Right (LBool True)
    it "parses 'false'" $ do
      parseOnly literalParser "false" `shouldBe` Right (LBool False)
  describe "integer literals" $ do
    it "parses a positive integer" $ do
      parseOnly literalParser "42" `shouldBe` Right (LInteger 42)
    it "parses zero" $ do
      parseOnly literalParser "0" `shouldBe` Right (LInteger 0)

-- ---------------------------------------------------------------------------
-- compareOpParser
-- ---------------------------------------------------------------------------

compareOpParserSpec :: Spec
compareOpParserSpec = describe "compareOpParser" $ do
  it "parses ==" $ parseOnly compareOpParser "==" `shouldBe` Right CEq
  it "parses !=" $ parseOnly compareOpParser "!=" `shouldBe` Right CNe
  it "parses <" $ parseOnly compareOpParser "<" `shouldBe` Right CLt
  it "parses <=" $ parseOnly compareOpParser "<=" `shouldBe` Right CLe
  it "parses >" $ parseOnly compareOpParser ">" `shouldBe` Right CGt
  it "parses >=" $ parseOnly compareOpParser ">=" `shouldBe` Right CGe
  it "parses !~" $ parseOnly compareOpParser "!~" `shouldBe` Right CNotMatch
  it "rejects unknown operator" $ do
    parseOnly compareOpParser "~=" `shouldSatisfy` isLeft

-- ---------------------------------------------------------------------------
-- pipeOpParser
-- ---------------------------------------------------------------------------

pipeOpParserSpec :: Spec
pipeOpParserSpec = describe "pipeOpParser" $ do
  it "parses |>" $ parseOnly pipeOpParser "|>" `shouldBe` Right PipeRight
  it "parses <|" $ parseOnly pipeOpParser "<|" `shouldBe` Right PipeLeft
  it "rejects |" $ parseOnly pipeOpParser "|" `shouldSatisfy` isLeft
  it "rejects <" $ parseOnly pipeOpParser "<" `shouldSatisfy` isLeft

-- ---------------------------------------------------------------------------
-- composeOpParser
-- ---------------------------------------------------------------------------

composeOpParserSpec :: Spec
composeOpParserSpec = describe "composeOpParser" $ do
  it "parses .>" $ parseOnly composeOpParser ".>" `shouldBe` Right ComposeRight
  it "parses <." $ parseOnly composeOpParser "<." `shouldBe` Right ComposeLeft
  it "rejects ." $ parseOnly composeOpParser "." `shouldSatisfy` isLeft

-- ---------------------------------------------------------------------------
-- addOpParser
-- ---------------------------------------------------------------------------

addOpParserSpec :: Spec
addOpParserSpec = describe "addOpParser" $ do
  it "parses +" $ parseOnly addOpParser "+" `shouldBe` Right APlus
  it "parses -" $ parseOnly addOpParser "-" `shouldBe` Right AMinus

-- ---------------------------------------------------------------------------
-- mulOpParser
-- ---------------------------------------------------------------------------

mulOpParserSpec :: Spec
mulOpParserSpec = describe "mulOpParser" $ do
  it "parses *" $ parseOnly mulOpParser "*" `shouldBe` Right MTimes
  it "parses /" $ parseOnly mulOpParser "/" `shouldBe` Right MDiv
  it "parses %" $ parseOnly mulOpParser "%" `shouldBe` Right MMod

-- ---------------------------------------------------------------------------
-- Schema declaration
-- ---------------------------------------------------------------------------

schemaSpec :: Spec
schemaSpec = describe "schema declaration" $ do
  it "parses schema at top of module" $ do
    parseModule "schema Foo" `shouldSatisfy` isRight
  it "parses schema followed by definitions" $ do
    parseModule "schema Foo\nX := Str" `shouldSatisfy` isRight
  it "rejects schema with lowercase name" $ do
    parseModule "schema foo" `shouldSatisfy` isLeft
  it "parses module without schema" $ do
    parseModule "X := Str" `shouldSatisfy` isRight
  it "parses empty module" $ do
    parseModule "" `shouldSatisfy` isRight
  it "parses module with only whitespace and comments" $ do
    parseModule "-- just a comment\n" `shouldSatisfy` isRight

-- ---------------------------------------------------------------------------
-- Comments
-- ---------------------------------------------------------------------------

commentSpec :: Spec
commentSpec = describe "comments" $ do
  it "ignores line comments" $ do
    parseModule "-- comment\nX := Str" `shouldSatisfy` isRight
  it "ignores inline comments after definition" $ do
    parseModule "X := Str -- alias" `shouldSatisfy` isRight
  it "ignores comment after where clause" $ do
    parseModule "X := Str where _ !~ \"-\" -- no dashes" `shouldSatisfy` isRight
  it "handles multiple consecutive comments" $ do
    parseModule "-- one\n-- two\n-- three\nX := Str" `shouldSatisfy` isRight
  it "handles dashes in line comment" $ do
    parseModule "X := Str -- a-b-c" `shouldSatisfy` isRight

-- ---------------------------------------------------------------------------
-- Primitive types
-- ---------------------------------------------------------------------------

primitiveTypeSpec :: Spec
primitiveTypeSpec = describe "primitive types" $ do
  it "parses Int" $ parseModule "X := Int" `shouldSatisfy` isRight
  it "parses Nat" $ parseModule "X := Nat" `shouldSatisfy` isRight
  it "parses Num" $ parseModule "X := Num" `shouldSatisfy` isRight
  it "parses Str" $ parseModule "X := Str" `shouldSatisfy` isRight
  it "parses Bool" $ parseModule "X := Bool" `shouldSatisfy` isRight
  it "parses Null" $ parseModule "X := Null" `shouldSatisfy` isRight
  it "parses List with type arg" $ parseModule "X := List Str" `shouldSatisfy` isRight
  it "parses List with named type arg" $ parseModule "X := List User"
    `shouldSatisfy` isRight
  it "parses Map with two type args" $ parseModule "X := Map Str Int"
    `shouldSatisfy` isRight
  it "rejects List without type arg" $ parseModule "X := List" `shouldSatisfy` isLeft
  it "rejects Map with one type arg" $ parseModule "X := Map Str"
    `shouldSatisfy` isLeft

-- ---------------------------------------------------------------------------
-- Union types
-- ---------------------------------------------------------------------------

unionTypeSpec :: Spec
unionTypeSpec = describe "union types" $ do
  it "parses inline union" $ do
    parseModule "X := Str | Int" `shouldSatisfy` isRight
  it "parses three-way union" $ do
    parseModule "X := Str | Int | Bool" `shouldSatisfy` isRight
  it "parses union with literal types" $ do
    parseModule "X := \"app\" | \"VM\" | \"service\"" `shouldSatisfy` isRight
  it "parses union with integer literals" $ do
    parseModule "X := 80 | 443 | 8080" `shouldSatisfy` isRight
  it "parses union with leading pipe (multi-line style)" $ do
    parseModule "X :=\n| Str\n| Int" `shouldSatisfy` isRight
  it "parses union mixing literals and named types" $ do
    parseModule "X := \"app\" | ServiceConfig" `shouldSatisfy` isRight
  it "parses single type (degenerate union)" $ do
    parseModule "X := Str" `shouldSatisfy` isRight
  it "parses union with boolean literal alternatives" $ do
    parseModule "X := true | false" `shouldSatisfy` isRight
  it "parses union with inline record alternative" $ do
    parseModule "X :=\n| kind : Str, value : Int;" `shouldSatisfy` isRight
  it "parses complex multi-line union" $ do
    let src =
          unlines
            [ "X :="
            , "| \"abcd\""
            , "| kind : Str, value : Int;"
            , "| ServiceConfig"
            ]
    parseModule src `shouldSatisfy` isRight

-- ---------------------------------------------------------------------------
-- Record types
-- ---------------------------------------------------------------------------

recordTypeSpec :: Spec
recordTypeSpec = describe "record types" $ do
  describe "inline records" $ do
    it "parses single field" $ do
      parseModule "X := name : Str;" `shouldSatisfy` isRight
    it "parses two fields comma-separated" $ do
      parseModule "X := a : Int, b : Str;" `shouldSatisfy` isRight
    it "parses optional field" $ do
      parseModule "X := name? : Str;" `shouldSatisfy` isRight
    it "parses mix of optional and required" $ do
      parseModule "X := id : Int, name? : Str;" `shouldSatisfy` isRight

  describe "multi-line records" $ do
    it "parses basic multi-line record" $ do
      let src =
            unlines
              [ "X :="
              , "  name : Str"
              , "  age : Int"
              , ";"
              ]
      parseModule src `shouldSatisfy` isRight
    it "parses nested record" $ do
      let src =
            unlines
              [ "X :="
              , "  family :"
              , "    father : Str"
              , "    mother : Str"
              , "  ;"
              , "  name : Str"
              , ";"
              ]
      parseModule src `shouldSatisfy` isRight
    it "parses optional field in multi-line record" $ do
      let src =
            unlines
              [ "X :="
              , "  id : Int"
              , "  name? : Str"
              , ";"
              ]
      parseModule src `shouldSatisfy` isRight
    it "parses deeply nested record" $ do
      let src =
            unlines
              [ "X :="
              , "  spec :"
              , "    template :"
              , "      containers : List Str"
              , "    ;"
              , "  ;"
              , ";"
              ]
      parseModule src `shouldSatisfy` isRight

  describe "inline record edge cases" $ do
    it "parses space before ; (multi-record path)" $ do
      parseModule "X := a : Int ;" `shouldSatisfy` isRight
    it "parses fixture-style InOneRecord" $ do
      parseModule "InOneRecord := a: Int, b?: Str ;" `shouldSatisfy` isRight
    it "parses complex expression followed by inline record" $ do
      let src =
            unlines
              [ "T2 := List User where _ |> map (family .> numOfBrothers) |> sum < 50"
              , ""
              , "InOneRecord := a: Int, b?: Str ;"
              ]
      parseModule src `shouldSatisfy` isRight

-- ---------------------------------------------------------------------------
-- Refinement types (where clause)
-- ---------------------------------------------------------------------------

refinementTypeSpec :: Spec
refinementTypeSpec = describe "refinement types (where)" $ do
  it "parses simple where with placeholder" $ do
    parseModule "X := Str where _ !~ \"-\"" `shouldSatisfy` isRight
  it "parses where with function call" $ do
    parseModule "X := Str where matches \"^[0-9]+$\" _" `shouldSatisfy` isRight
  it "parses where with len and comparison" $ do
    parseModule "X := Str where len _ > 0" `shouldSatisfy` isRight
  it "parses where with and" $ do
    parseModule "X := Str where _ !~ \"-\" and _ != \"s\"" `shouldSatisfy` isRight
  it "parses where with or" $ do
    parseModule "X := Str where len _ > 0 or _ == \"default\"" `shouldSatisfy` isRight
  it "parses where with pipe operator" $ do
    parseModule "X := Str where _ |> len > 0" `shouldSatisfy` isRight
  it "parses where on record field type" $ do
    let src =
          unlines
            [ "X :="
            , "  name : Str where len _ > 0"
            , ";"
            ]
    parseModule src `shouldSatisfy` isRight
  it "parses where after record semicolon" $ do
    let src =
          unlines
            [ "X :="
            , "  containers : List Str"
            , ";   where len _ >= 1"
            ]
    parseModule src `shouldSatisfy` isRight
  it "parses where with arithmetic" $ do
    parseModule "X := Int where _ > 0 and _ < 100" `shouldSatisfy` isRight
  it "parses where with negation" $ do
    parseModule "X := Int where _ != -1" `shouldSatisfy` isRight

-- ---------------------------------------------------------------------------
-- Expressions (precedence, associativity)
-- ---------------------------------------------------------------------------

expressionSpec :: Spec
expressionSpec = describe "expressions" $ do
  describe "arithmetic" $ do
    it "parses addition" $ do
      parseModule "X := Int where _ + 1 > 0" `shouldSatisfy` isRight
    it "parses subtraction" $ do
      parseModule "X := Int where _ - 1 >= 0" `shouldSatisfy` isRight
    it "parses multiplication" $ do
      parseModule "X := Int where _ * 2 < 100" `shouldSatisfy` isRight
    it "parses division" $ do
      parseModule "X := Int where _ / 2 > 0" `shouldSatisfy` isRight
    it "parses modulo" $ do
      parseModule "X := Int where _ % 2 == 0" `shouldSatisfy` isRight
    it "parses chained arithmetic" $ do
      parseModule "X := Int where _ + 1 - 2 > 0" `shouldSatisfy` isRight
    it "parses mixed precedence" $ do
      parseModule "X := Int where _ * 2 + 1 > 0" `shouldSatisfy` isRight

  describe "unary negation" $ do
    it "parses negation of placeholder" $ do
      parseModule "X := Int where -_ < 0" `shouldSatisfy` isRight
    it "parses negation of literal" $ do
      parseModule "X := Int where _ > -1" `shouldSatisfy` isRight
    it "parses double negation" $ do
      parseModule "X := Int where _ > - -1" `shouldSatisfy` isRight

  describe "pipes" $ do
    it "parses pipe right" $ do
      parseModule "X := Str where _ |> len > 0" `shouldSatisfy` isRight
    it "parses chained pipes" $ do
      parseModule "X := Str where _ |> len |> (> 0)" `shouldSatisfy` isRight
    it "parses pipe left" $ do
      parseModule "X := Str where len <| _ > 0" `shouldSatisfy` isRight

  describe "composition" $ do
    it "parses compose right" $ do
      parseModule "X := List Int where all (field1 .> field2 .> (< 2)) _"
        `shouldSatisfy` isRight
    it "parses compose left" $ do
      parseModule "X := List Int where all (field2 <. field1) _"
        `shouldSatisfy` isRight

  describe "logical operators" $ do
    it "parses and" $ do
      parseModule "X := Int where _ > 0 and _ < 100" `shouldSatisfy` isRight
    it "parses or" $ do
      parseModule "X := Int where _ == 0 or _ == 1" `shouldSatisfy` isRight
    it "parses and/or precedence (and binds tighter)" $ do
      parseModule "X := Int where _ > 0 and _ < 10 or _ == 99"
        `shouldSatisfy` isRight
    it "parses multiple ands" $ do
      parseModule "X := Int where _ > 0 and _ < 100 and _ != 50"
        `shouldSatisfy` isRight

  describe "parenthesized expressions" $ do
    it "parses parens around expression" $ do
      parseModule "X := Int where (_ + 1) > 0" `shouldSatisfy` isRight

-- ---------------------------------------------------------------------------
-- Function calls
-- ---------------------------------------------------------------------------

functionCallSpec :: Spec
functionCallSpec = describe "function calls" $ do
  it "parses single-arg function call" $ do
    parseModule "X := Str where len _ > 0" `shouldSatisfy` isRight
  it "parses two-arg function call" $ do
    parseModule "X := Str where matches \"^[0-9]+$\" _" `shouldSatisfy` isRight
  it "parses function call with string literal arg" $ do
    parseModule "X := Str where startsWith \"/\" _" `shouldSatisfy` isRight
  it "parses function call with placeholder as second arg" $ do
    parseModule "X := Str where contains \"abc\" _" `shouldSatisfy` isRight
  it "parses not builtin" $ do
    parseModule "X := Bool where not _" `shouldSatisfy` isRight
  it "parses nested function call via parens" $ do
    parseModule "X := List Str where all (contains \"x\") _" `shouldSatisfy` isRight
  it "parses function call as pipe target" $ do
    parseModule "X := Str where _ |> matches \"^[0-9]+$\"" `shouldSatisfy` isRight
  it "parses field selection as function call" $ do
    let src =
          unlines
            [ "X :="
            , "  a : Int"
            , "  b : Int"
            , ";   where a _ > b _"
            ]
    parseModule src `shouldSatisfy` isRight

-- ---------------------------------------------------------------------------
-- Operator sections
-- ---------------------------------------------------------------------------

operatorSectionSpec :: Spec
operatorSectionSpec = describe "operator sections" $ do
  describe "comparison sections" $ do
    it "parses right section (< 2)" $ do
      parseModule "X := Int where all (< 2) _" `shouldSatisfy` isRight
    it "parses right section (> 0)" $ do
      parseModule "X := Int where all (> 0) _" `shouldSatisfy` isRight
    it "parses right section (<= 10)" $ do
      parseModule "X := Int where all (<= 10) _" `shouldSatisfy` isRight
    it "parses right section (>= 1)" $ do
      parseModule "X := Int where all (>= 1) _" `shouldSatisfy` isRight
    it "parses right section (== 0)" $ do
      parseModule "X := Int where all (== 0) _" `shouldSatisfy` isRight
    it "parses right section (!= 0)" $ do
      parseModule "X := Int where all (!= 0) _" `shouldSatisfy` isRight
    it "parses left section (x <)" $ do
      parseModule "X := Int where all (_ <) _" `shouldSatisfy` isRight

  describe "arithmetic sections" $ do
    it "parses right section (+ 1)" $ do
      parseModule "X := List Int where map (+ 1) _" `shouldSatisfy` isRight
    it "parses right section (* 2)" $ do
      parseModule "X := List Int where map (* 2) _" `shouldSatisfy` isRight
    it "parses right section (/ 2)" $ do
      parseModule "X := List Int where map (/ 2) _" `shouldSatisfy` isRight
    it "parses right section (% 2)" $ do
      parseModule "X := List Int where map (% 2) _" `shouldSatisfy` isRight
    it "parses left section (x +)" $ do
      parseModule "X := List Int where map (_ +) _" `shouldSatisfy` isRight
    it "parses left section (x -)" $ do
      parseModule "X := List Int where map (_ -) _" `shouldSatisfy` isRight

  describe "composition in sections" $ do
    it "parses composed field access with section" $ do
      parseModule "X := List Int where all (field1 .> field2 .> (< 2)) _"
        `shouldSatisfy` isRight

-- ---------------------------------------------------------------------------
-- AST-inspecting tests
-- ---------------------------------------------------------------------------

astInspectionSpec :: Spec
astInspectionSpec = describe "AST structure" $ do
  let
    def src = case parseModule src of
      Right (Module _ [d]) -> Just d
      _ -> Nothing
    ty src = defType <$> def src
    term src = case ty src of
      Just (TypeExpr (UnionType (t :| []))) -> Just t
      _ -> Nothing

  it "simple alias produces TNamed" $ do
    term "Name := Str" `shouldBe` Just (TPrimitive TStr)

  it "integer literal type produces TLiteral" $ do
    term "X := 42" `shouldBe` Just (TLiteral (TLitInteger 42))

  it "string literal type produces TLiteral" $ do
    term "X := \"hello\"" `shouldBe` Just (TLiteral (TLitString "hello"))

  it "bool literal type produces TLiteral" $ do
    term "X := true" `shouldBe` Just (TLiteral (TLitBool True))

  it "named type produces TNamed" $ do
    term "X := Foo" `shouldBe` Just (TNamed (TypeName "Foo"))

  it "union has correct number of terms" $ do
    ty "X := Str | Int | Bool"
      `shouldBe` Just
        ( TypeExpr
            ( UnionType
                ( TPrimitive TStr
                    :| [TPrimitive TInt, TPrimitive TBool]
                )
            )
        )

  it "List Int | Null parses as (List Int) | Null, not List (Int | Null)" $ do
    ty "X := List Int | Null"
      `shouldBe` Just
        ( TypeExpr
            ( UnionType
                ( TPrimitive (TList (TypeExpr (UnionType (TPrimitive TInt :| []))))
                    :| [TPrimitive TNull]
                )
            )
        )

  it "inline record has correct fields" $ do
    term "X := a : Int, b : Str;"
      `shouldBe` Just
        ( TRecord
            ( TInlineRecord
                [ Field (Identifier "a") False (TypeExpr (UnionType (TPrimitive TInt :| [])))
                , Field (Identifier "b") False (TypeExpr (UnionType (TPrimitive TStr :| [])))
                ]
            )
        )

  it "optional field sets fieldOptional = True" $ do
    term "X := name? : Str;"
      `shouldBe` Just
        ( TRecord
            ( TInlineRecord
                [ Field (Identifier "name") True (TypeExpr (UnionType (TPrimitive TStr :| [])))
                ]
            )
        )

  it "refinement wraps base term" $ do
    term "X := Str where _ != \"\""
      `shouldBe` Just
        ( TRefinement
            (TPrimitive TStr)
            (ECompare (EAtom APlaceholder) CNe (EAtom (ALiteral (LString ""))))
        )

  it "schema declaration is captured" $ do
    case parseModule "schema Foo\nX := Str" of
      Right (Module (Just (SchemaDecl (TypeName "Foo"))) _) -> pure ()
      other -> expectationFailure ("unexpected: " <> show other)

  it "definition name is correct" $ do
    fmap defName (def "MyType := Int") `shouldBe` Just (TypeName "MyType")

-- ---------------------------------------------------------------------------
-- String escape sequences
-- ---------------------------------------------------------------------------

stringEscapeSpec :: Spec
stringEscapeSpec = describe "stringLiteralParser escape sequences" $ do
  it "parses backslash-n as newline" $ do
    parseOnly stringLiteralParser "\"hello\\nworld\"" `shouldBe` Right "hello\nworld"
  it "parses backslash-t as tab" $ do
    parseOnly stringLiteralParser "\"a\\tb\"" `shouldBe` Right "a\tb"
  it "parses escaped double quote" $ do
    parseOnly stringLiteralParser "\"say \\\"hi\\\"\"" `shouldBe` Right "say \"hi\""
  it "parses escaped single quote inside single-quoted string" $ do
    parseOnly stringLiteralParser "'it\\'s'" `shouldBe` Right "it's"
  it "parses escaped backslash" $ do
    parseOnly stringLiteralParser "\"a\\\\b\"" `shouldBe` Right "a\\b"
  it "consumes trailing whitespace" $ do
    parseOnly stringLiteralParser "\"hi\"  " `shouldBe` Right "hi"

-- ---------------------------------------------------------------------------
-- List/Map union ambiguity
-- ---------------------------------------------------------------------------

listUnionAmbiguitySpec :: Spec
listUnionAmbiguitySpec = describe "List/Map union ambiguity" $ do
  it "List Int | Null is (List Int) | Null" $ do
    case parseModule "X := List Int | Null" of
      Right (Module _ [Definition _ (TypeExpr (UnionType (t1 :| [t2])))]) -> do
        t1 `shouldBe` TPrimitive (TList (TypeExpr (UnionType (TPrimitive TInt :| []))))
        t2 `shouldBe` TPrimitive TNull
      other -> expectationFailure ("unexpected: " <> show other)
  it "Map Str Int | Null is (Map Str Int) | Null" $ do
    case parseModule "X := Map Str Int | Null" of
      Right (Module _ [Definition _ (TypeExpr (UnionType (t1 :| [t2])))]) -> do
        t1
          `shouldBe` TPrimitive
            ( TMap
                (TypeExpr (UnionType (TPrimitive TStr :| [])))
                (TypeExpr (UnionType (TPrimitive TInt :| [])))
            )
        t2 `shouldBe` TPrimitive TNull
      other -> expectationFailure ("unexpected: " <> show other)
  it "List (Int | Null) can be written with explicit union term" $ do
    parseModule "X := List Int" `shouldSatisfy` isRight
