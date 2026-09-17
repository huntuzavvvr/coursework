namespace Mosaic

type Position = { Offset: int; Line: int; Column: int }
type Span = { Start: Position; Finish: Position }
type Diagnostic = { Code: string; Message: string; Span: Span }

module Diagnostic =
    let origin = { Offset = 0; Line = 1; Column = 1 }
    let nowhere = { Start = origin; Finish = origin }
    let make code message span = { Code = code; Message = message; Span = span }
    let format sourceName diagnostic =
        $"{sourceName}:{diagnostic.Span.Start.Line}:{diagnostic.Span.Start.Column}: {diagnostic.Code}: {diagnostic.Message}"

/// Data that can be printed as the result of a program.
[<StructuralEquality; StructuralComparison>]
type Data =
    | Number of bigint
    | Boolean of bool
    | Text of string
    | Sequence of Data list

module Data =
    let quote (text: string) = System.Text.Json.JsonSerializer.Serialize text
    let rec format = function
        | Number n -> string n
        | Boolean b -> if b then "true" else "false"
        | Text s -> quote s
        | Sequence items -> "[" + (items |> List.map format |> String.concat " ") + "]"

type Expr = { Node: Node; Span: Span }
and Node =
    | Literal of Data
    | Variable of string
    | Lambda of string list * Expr
    | Apply of Expr * Expr list
    | Bind of (string * Expr) list * Expr
    | Recursive of (string * string list * Expr) list * Expr
    | Conditional of Expr * Expr * Expr
    | ListExpr of Expr list
    | Delay of Expr
    | Force of Expr
    | Pipeline of Expr * Expr list

