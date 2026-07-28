# Refina

A refinement-type-based DSL for validating JSON/YAML config files (Haskell).

## Build & Test

```sh
cabal build                                     # build library + executable
cabal test                                      # run all tests (hspec + hedgehog)
cabal test --test-option='-p "record"'          # single test by name pattern
cabal test --test-option='--match=parser'       # parser tests only
cabal test --test-option='--match=typeche'      # type checker tests only
cabal test --test-option='--match=Hedgehog'     # hedgehog property tests only
cabal run refina -- test/fixtures/simple.refina # parse & type-check a .refina file
cabal build --ghc-options=-fno-code             # fast type-check only (no codegen)
cabal repl                                      # GHCi with library + app
```

## Formatting

Fourmolu is the mandatory formatter. Config is in `fourmolu.yaml` (2-space indent, 85-col limit). A pre-commit hook auto-formats staged `.hs` files.

```sh
fourmolu --mode inplace <space-separated-edited-files>
```

## Implementation Order

1. **AST** (`Refina.AST`) — define the syntax tree types that mirror the EBNF (`docs/grammar.ebnf`). Every grammar production gets a corresponding type.
2. **Parser** (`Refina.Parser`) — megaparsec parser producing AST. Start with lexer tokens, then full grammar.
3. **Type checker** (`Refina.TypeChecker`) — resolve named types, validate record fields, check refinements are well-typed.
4. **Predicate evaluator** (`Refina.Predicate`) — evaluate `where` clauses against concrete values (JSON/YAML).
5. **Validator** (`Refina.Validator`) — type-check a schema + validate an input value against it.
6. **Input loaders** (`Refina.Input.JSON`, `Refina.Input.YAML`) — parse input files into the internal value representation.
7. **CLI** (`Refina.CLI`) — wire schema file + input file → validation result with diagnostics.

**Important**:
- **Tests** — write as you go alongside each stage.

**Note**:
- **`app/Main.hs`** — CLI entry point.

## Current State

- [ ] Parser and AST
- [ ] Type checker
- [ ] Predicate evaluator
- [ ] Validator
- [ ] Input loaders
- [ ] CLI

## Code Conventions

- **Module naming**: `Refina.StageName` — one concept per module, avoid large files
- **Smart constructors** for complex AST nodes where invariants must be enforced
- **Tests**: `test/Refina/<ModuleName>Spec.hs` mirrors `src/Refina/<ModuleName>.hs`
- **Prelude**: `relude` (NoImplicitPrelude is enabled) — import explicitly

## Reference Docs

- `docs/grammar.ebnf` — formal EBNF grammar (source of truth for parser)
- `docs/semantics.md` — language semantics reference
- `docs/syntax` — example .refina file showing all language features

