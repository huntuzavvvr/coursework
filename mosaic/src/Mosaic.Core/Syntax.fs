namespace Mosaic

open System.Numerics

/// A canonical rational number; construction is confined to Rational.create.
[<StructuralEquality; StructuralComparison>]
type Rational = private { Numerator: bigint; Denominator: bigint }

module Rational =
    let create numerator denominator =
        if denominator = 0I then invalidArg "denominator" "Zero denominator"
        let sign = if denominator < 0I then -1I else 1I
        let divisor = BigInteger.GreatestCommonDivisor(BigInteger.Abs numerator, BigInteger.Abs denominator)
        { Numerator = sign * numerator / divisor; Denominator = sign * denominator / divisor }

    let integer n = create n 1I
    let zero = integer 0I
    let one = integer 1I
    let add a b = create (a.Numerator * b.Denominator + b.Numerator * a.Denominator) (a.Denominator * b.Denominator)
    let negate a = { a with Numerator = -a.Numerator }
    let subtract a b = add a (negate b)
    let multiply a b = create (a.Numerator * b.Numerator) (a.Denominator * b.Denominator)
    let divide a b = create (a.Numerator * b.Denominator) (a.Denominator * b.Numerator)
    let compare a b = compare (a.Numerator * b.Denominator) (b.Numerator * a.Denominator)
    let asInteger a = if a.Denominator = 1I then Some a.Numerator else None
    let format a =
        if a.Denominator = 1I then string a.Numerator
        else $"{a.Numerator}/{a.Denominator}"

type Position = { Offset: int; Line: int; Column: int }
type Span = { Start: Position; Finish: Position }
type Diagnostic = { Code: string; Message: string; Span: Span }

module Diagnostic =
    let origin = { Offset = 0; Line = 1; Column = 1 }
    let nowhere = { Start = origin; Finish = origin }
    let make code message span = { Code = code; Message = message; Span = span }
    let format sourceName diagnostic =
        $"{sourceName}:{diagnostic.Span.Start.Line}:{diagnostic.Span.Start.Column}: {diagnostic.Code}: {diagnostic.Message}"

/// Only data can be a choice outcome or an observable final result.
[<StructuralEquality; StructuralComparison>]
type Data =
    | Number of Rational
    | Boolean of bool
    | Text of string
    | Sequence of Data list

module Data =
    let quote (text: string) = System.Text.Json.JsonSerializer.Serialize text
    let rec format = function
        | Number n -> Rational.format n
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
    | Observe of Expr * Expr

