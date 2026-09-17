namespace Mosaic

type FunctionDefinition = string * string list * Expr

/// Recursive closures capture descriptions, not a reference to their own environment.
[<NoEquality; NoComparison>]
type Value =
    | Scalar of Data
    | Items of Value list
    | Closure of string list * Expr * Map<string, Value>
    | RecursiveClosure of string * FunctionDefinition list * Map<string, Value>
    | Suspended of Expr * Map<string, Value>
    | Primitive of string * Value list

module Value =
    let rec ofData = function
        | Sequence items -> Items (List.map ofData items)
        | data -> Scalar data

    let rec toData = function
        | Scalar data -> Ok data
        | Items items ->
            let folder result item =
                match result, toData item with
                | Ok reversed, Ok data -> Ok (data :: reversed)
                | Error message, _ | _, Error message -> Error message
            List.fold folder (Ok []) items |> Result.map (List.rev >> Sequence)
        | Closure _ | RecursiveClosure _ | Primitive _ -> Error "A function cannot be printed as data"
        | Suspended _ -> Error "A delayed expression must be forced before printing it as data"

type Context = { Inputs: Map<string, string> }
type Report = { Value: Data; Outputs: Map<string, string> }
