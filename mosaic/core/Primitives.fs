namespace Mosaic

/// File operations use text supplied by the host and return pending outputs.
module Primitives =
    let arities =
        ["+", 2; "-", 2; "*", 2; "/", 2; "mod", 2
         "=", 2; "<", 2; "<=", 2; ">", 2; ">=", 2
         "not", 1; "cons", 2; "head", 1; "tail", 1; "empty?", 1; "length", 1
         "append", 2; "text-append", 2; "show", 1
         "read-text", 1; "write-text", 2] |> Map.ofList

    let environment = arities |> Map.map (fun name _ -> Primitive (name, []))
    let private number n = Scalar (Number n)
    let private boolean b = Scalar (Boolean b)
    let private text s = Scalar (Text s)
    let private error code message = Error (code, message)

    let invoke context span name args outputs : Result<Value * Map<string, string>, Diagnostic> =
        let single value = Ok (value, outputs)
        let result =
            match name, args with
            | "+", [Scalar (Number a); Scalar (Number b)] -> single (number (a + b))
            | "-", [Scalar (Number a); Scalar (Number b)] -> single (number (a - b))
            | "*", [Scalar (Number a); Scalar (Number b)] -> single (number (a * b))
            | ("/" | "mod"), [Scalar (Number _); Scalar (Number b)] when b = 0I -> error "E_ZERO" "Division by zero"
            | "/", [Scalar (Number a); Scalar (Number b)] -> single (number (a / b))
            | "mod", [Scalar (Number a); Scalar (Number b)] -> single (number (a % b))
            | ("<" | "<=" | ">" | ">="), [Scalar (Number a); Scalar (Number b)] ->
                single (boolean (match name with "<" -> a < b | "<=" -> a <= b | ">" -> a > b | _ -> a >= b))
            | "=", [a; b] ->
                match Value.toData a, Value.toData b with
                | Ok x, Ok y -> single (boolean (x = y))
                | Error message, _ | _, Error message -> error "E_TYPE" message
            | "not", [Scalar (Boolean b)] -> single (boolean (not b))
            | "cons", [item; Items items] -> single (Items (item :: items))
            | "head", [Items (head :: _)] -> single head
            | "tail", [Items (_ :: tail)] -> single (Items tail)
            | ("head" | "tail"), [Items []] -> error "E_EMPTY" $"{name} of an empty list"
            | "empty?", [Items items] -> single (boolean (List.isEmpty items))
            | "length", [Items items] -> single (number (bigint (List.length items)))
            | "append", [Items a; Items b] -> single (Items (a @ b))
            | "text-append", [Scalar (Text a); Scalar (Text b)] -> single (text (a + b))
            | "show", [value] ->
                match Value.toData value with
                | Ok data -> single (text (Data.format data))
                | Error message -> error "E_TYPE" message
            | "read-text", [Scalar (Text alias)] ->
                match Map.tryFind alias context.Inputs with
                | Some contents -> single (text contents)
                | None -> error "E_INPUT" $"Input '{alias}' was not supplied by the host"
            | "write-text", [Scalar (Text alias); Scalar (Text contents)] ->
                match Map.tryFind alias outputs with
                | Some previous when previous <> contents -> error "E_OUTPUT" $"Output '{alias}' already has different contents"
                | _ -> Ok (text contents, Map.add alias contents outputs)
            | _ -> error "E_TYPE" $"Invalid argument types for '{name}'"
        result |> Result.mapError (fun (code, message) -> Diagnostic.make code message span)
