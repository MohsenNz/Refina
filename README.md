# Refina

A refinement-type-based DSL for validating structured configuration.

## Overview

**Refina** is a declarative language for describing and validating configuration data. It combines structural types, refinement predicates, algebraic data types, and composable expressions to define not only the shape of data, but also the semantic rules that make configurations valid.

## Examples

```
schema Deployment

Deployment :=
  apiVersion  : "apps/v1"
  kind        : "Deployment"
  metadata?   :
    name      : Str where _ !~ "/" and len _ > 0
  ;
  spec?       :
    replicas? : Int where _ >= 1
    template? :
      spec? :
        containers : List Container where len _ >= 1
      ;
    ;
  ;
;

Container :=
  name       : Str
  image      : Str
  resources? :
    requests :
      cpu    : Nat
      memory : Nat
    ;
    limits:
      cpu    : Nat
      memory : Nat
    ;
    ab: Str
  ;
  where _ |> limits |> cpu    >= _ |> requests |> cpu
    and _ |> limits |> memory >= _ |> requests |> memory
;
```

## Features

* Structural type system
* Refinement types (`where`)
* Record and union types
* Optional fields (`?`)
* Literal types
* Lists and maps
* Cross-field validation
* JSON and YAML validation
* Human-readable diagnostics

## Design Philosophy

A schema denotes a **type**. A configuration is valid if its value belongs to that type and satisfies all refinement predicates.

Refinement types are written as:

```dsl
UserId := Str where _ !~ "-"
```

where `_` denotes the value being refined.

## Build & Run

```sh
nix develop                                         # enter to dev env
cabal build                                         # build
cabal run refina -- test/fixtures/simple.refina     # parse a .refina file
cabal test                      # run all tests (hspec + hedgehog)
cabal test --test-option='--match=parser'  # run only parser tests
cabal test --test-option='--match=typeche' # run only type checker tests
cabal test --test-option='--match=Hedgehog' # run only hedgehog property tests
cabal test --test-option='-p "record"'  # single test
```

## Roadmap

* [x] Parser and AST
* [ ] Type checker
* [ ] Predicate evaluator
* [ ] JSON/YAML validator
* [ ] CLI
* [ ] JSONSchema generator
* [ ] Language Server Protocol (LSP)
* [ ] Kubernetes integration
<!-- * [ ] OpenAPI support -->
