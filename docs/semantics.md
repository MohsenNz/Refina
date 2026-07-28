Refina — Language Semantics
============================

This document describes the semantic rules of the Refina DSL.
For syntax and grammar, see `docs/syntax` and `docs/grammar.ebnf`.

0. Schema Entrypoint
--------------------

`schema AType` at the top level declares `AType` as the schema entry point,
the top-level type that input data is validated against. At most one
`schema` declaration is allowed per file, and that type should be defined.

    schema AType

1. Comments
-----------

Comments start with `--` and extend to the end of the line:

    -- this is a comment

2. Type Definition
------------------

Type definition is done with `:=`.

    MyType := Str

3. Types
--------

Type can be described like this in pseudo-code:

    Type :=
    | Primitive
    | Record     -- record type: @name : Str, age : Int ;@
    | Union
    | Literal    -- singleton literal type: @"app"@, @42@, @true@

    Primitive :=
    | Int        -- integer numbers
    | Nat        -- not negative numbers, 0, 1, 2, ...
    | Num        -- floating-point or integer numbers
    | Str        -- strings, it is a representation of (List Char)
    | Bool       -- boolean
    | List t     -- homogeneous sequences
    | Map t1 t2  -- represent arbitrary object in json/yaml
    | Null       -- represent null value in json/yaml

    Literal :=
    | StringLiteral
    | IntegerLiteral
    | BooleanLiteral

4. Record Types
---------------

Record contains multiple field and ended with `;`.
The comma `,` between fields is needed for single line record. E.g. here:

    User :=
      family:
        father: Str
        mother: Str
        numOfBrothers: Int
      ;
      name: Str
    ;

    OneLineRecord := a: Int, b: Str ;

Can be represented like this in TypeScript:

    type User = {
      family: {
        father: string,
        mother: string,
        numOfBrothers: Int
      }
      name: string
    }

    type OneLineRecord = { a: Int, b: Str}

A field marked with `?` is optional - its absence is valid.
If present, its value must match the declared
type; if absent, no error is raised. Its like `t | undefined` in TypeScript.

    User :=
      id    : UserId -- required
      name? : Str    -- optional
    ;

5. Union Types
--------------

A union `A | B | C` is tried in order. The first alternative whose type
matches the input wins:

    Config := AppConfig | ServiceConfig

Literal alternatives can be mixed with named or structural ones and be in multiple lines:

    MyUnion :=
    | "abcd"
    | kind : Kind, value : Int;  -- one line record in union type
    | f1 : Str                   -- mulily line record in union type
      f2 : Int
      f3 : Int
    ;
    | 4
    | 10
    | ServiceConfig

6. Refinement Types
-------------------

Refinement Types allow a type to be constrained with additional predicates. A refinement type starts from an existing base type and narrows the set of valid values by requiring a condition to hold. The `where` clause attaches a predicate to a type, and `_` (placeholder) represents the value being checked.

    UserId := Str where _ !~ "-" -- `_` points to Str

    Cpu := Str where matches "^[0-9]+m?$" _

A `where` clause can appear here `t where <predicate>` where `t` is a type,
and `<predicate>` is an expressions which return `Bool`.

Examples:

    User :=
      id    : UserId
      name? : Str where len _ > 0   -- `_` point to Str
    ;

    PodSpec :=
      containers : List Container
    ;   where len (containers _) >= 1
    -- `_` points to { containers : List Container }

7. Literal Types
----------------

A literal value used as a type defines a *singleton type* — only the exact
same value passes validation. String, number, and boolean literals are
supported:

    Kind := "app" | "VM" | "service" -- only these three strings
    Port := 8080 | 443               -- only these two numbers
    Flag := true                     -- only the boolean true

8. Non-recursive
----------------

Type definition must be acyclic — recursive are not allowed:

    Name := Str   -- ok
    Foo  := Bar   -- ok
    Bar  := Foo   -- ERROR: cyclic

9. Str Type
-----------

String is a list of characters, then it would be:

    Str := List Char

  But we don't expose `Char`.

10. String Literals
-------------------

String literals accept both single and double quotes:

    "hello world"
    'hello world'

11. Built-in Functions
----------------------

Lists or Strings:

    matches     :: List t -- Parttern
                -> List t -- Input
                -> Bool

    startsWith  :: List t -- Prefix
                -> List t -- Input
                -> Bool

    endsWith    :: List t -- Suffix
                -> List t -- Input
                -> Bool

    contains    :: List t -- Sublist
                -> List t -- Input
                -> Bool

    member      :: t      -- Element
                -> List t -- Input
                -> Bool   -- true if the element exists in the list

    all         :: (t -> Bool) -- Predicate
                -> List t      -- Input
                -> Bool        -- true if every element satisfies the predicate

    any         :: (t -> Bool) -- Predicate
                -> List t -- List
                -> Bool   -- true if at least one element satisfies the predicate

    map         :: (t1 -> t2) -- Transformation function
                -> List t1    -- Input
                -> List t2    -- transformed list

    len         :: List t  -- Input
                -> Int     -- result of length of the list

General:

    not :: Bool -> Bool

Function call is like this:

    len "sdfsf"
    matches "^[0-9]+m?$" "8mjsfk"

Built-in functions get used in predicates. E.g:

    Cpu := Str where matches "^[0-9]+m?$" _

12. Partial Application
-----------------------

A function may be called with fewer arguments than its full parameter list.
The result is a partially applied function awaiting the remaining arguments.

    matches "^[0-9]+$"             -- partial: expects the string to test
    "8mjsfk" |> matches "^[0-9]+$" -- pipe the value into the partial application

13. Field Selection
-------------------

In a `where` clause, non-builtin functions are functions to select a field. E.g. 
`abc` is equivalent to `fun x => x.abc` — you can select the `abc` field with `abc _`. 


Examples:

    Container :=
      resources? :
        requests : cpu : Cpu ;
        limits   : cpu : Cpu ;
      ;
    ;
    where cpu (limits _) >= cpu (requests _)

14. Pipe Operators
------------------

`|>` (pipe-right) and `<|` (pipe-left) thread a value through a function:

    _ |> f          -- apply f to _    (same as f _)
    f <| _          -- apply f to _    (same as f _)

Pipes are left-associative and have lower precedence than comparisons.
They are the lowest-precedence operators inside value expressions.

Combining with the pipe operator `|>`, this gives nested field access similar to dot notation in other languages:

    _ |> limits |> cpu

is equivalent to  limits (cpu _)

    Container :=
      resources? :
        requests : cpu : Cpu ;
        limits   : cpu : Cpu ;
      ;
    ;
    where _ |> limits |> cpu >= _ |> limits |> cpu

15. Function Composition
------------------------

`.>` (left-to-right) and `<.` (right-to-left) compose two functions:

    (f .> g) x   --  g (f x)
    (f <. g) x   --  f (g x)

Composition binds tighter than pipes but looser than arithmetic operators.


16. Operator Sections
---------------------

An operator can be partially applied as a left or right section:

    (< 2)     --  \x -> x < 2       (right section)
    (x <)     --  \y -> x < y       (left section)

This works for all comparison and arithmetic operators.
The `-` operator in a right-section position is parsed as unary negation
instead, use a left section `(x -)` to get the subtraction section.

Example:

    H :=
      field1 :
        abcd : Int
      ;
      field2 : Int
    ;
    where all (field1 .> abcd .> (< 2)) _

17. Operator Precedence (lowest to tightest)
---------------------------------------------

    or                    LogicalExpression
    and                   LogicalTerm
    == != < <= > >= !~    ComparisonOperator
    |> <|                 PipeExpression
    .> <.                 FunctionComposition
    + -                   ArithmeticExpression (additive)
    * / %                 Term (multiplicative)
    -x                    Unary (negation)
    atom                  Factor (literals, identifiers, calls, sections, parens)
