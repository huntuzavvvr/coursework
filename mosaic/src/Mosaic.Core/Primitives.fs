namespace Mosaic

/// These operations return values/worlds. They never perform host I/O.
module Primitives =
    let arities =
        ["+", 2; "-", 2; "*", 2; "/", 2; "mod", 2
         "=", 2; "<", 2; "<=", 2; ">", 2; ">=", 2
         "not", 1; "cons", 2; "head", 1; "tail", 1; "empty?", 1; "length", 1
         "append", 2; "text-append", 2; "show", 1; "choose", 2
         "read-text", 1; "write-text", 2] |> Map.ofList

    let environment = arities |> Map.map (fun name _ -> Primitive (name, []))
    let private number n = Scalar (Number n)
    let private boolean b = Scalar (Boolean b)
    let private text s = Scalar (Text s)
    let private error code message = Error (code, message)

    let private distribution alternatives =
        let folder result alternative =
            match result, alternative with
            | Error e, _ -> Error e
            | Ok reversed, Items [Scalar (Number weight); value] ->
                if Rational.compare weight Rational.zero < 0 then error "E_WEIGHT" "A choice weight must be nonnegative"
                else
                    match Value.toData value with
                    | Error message -> error "E_CHOICE" message
                    | Ok data -> Ok ((weight, data) :: reversed)
            | _ -> error "E_CHOICE" "Choice alternatives must be [weight value] pairs"
        List.fold folder (Ok []) alternatives
        |> Result.bind (fun reversed ->
            let pairs = List.rev reversed
            let total = pairs |> List.fold (fun acc (weight, _) -> Rational.add acc weight) Rational.zero
            if total = Rational.zero then error "E_WEIGHT" "A choice needs positive total weight"
            else
                // Merge duplicate outcomes. Equivalent reordered/scaled distributions share an identity.
                pairs
                |> List.fold (fun grouped (weight, value) ->
                    let previous = Map.tryFind value grouped |> Option.defaultValue Rational.zero
                    Map.add value (Rational.add previous weight) grouped) Map.empty
                |> Map.toList
                |> List.choose (fun (value, weight) ->
                    if weight = Rational.zero then None else Some (Rational.divide weight total, value))
                |> Ok)

    let invoke context span name args (world: World) : Result<(Value * World) list, Diagnostic> =
        let single value = Ok [value, world]
        let result =
            match name, args with
            | "+", [Scalar (Number a); Scalar (Number b)] -> single (number (Rational.add a b))
            | "-", [Scalar (Number a); Scalar (Number b)] -> single (number (Rational.subtract a b))
            | "*", [Scalar (Number a); Scalar (Number b)] -> single (number (Rational.multiply a b))
            | "/", [Scalar (Number _); Scalar (Number b)] when b = Rational.zero -> error "E_ZERO" "Division by zero"
            | "/", [Scalar (Number a); Scalar (Number b)] -> single (number (Rational.divide a b))
            | "mod", [Scalar (Number a); Scalar (Number b)] ->
                match Rational.asInteger a, Rational.asInteger b with
                | Some _, Some y when y = 0I -> error "E_ZERO" "Remainder by zero"
                | Some x, Some y -> single (number (Rational.integer (x % y)))
                | _ -> error "E_TYPE" "mod expects integers"
            | ("<" | "<=" | ">" | ">="), [Scalar (Number a); Scalar (Number b)] ->
                let order = Rational.compare a b
                single (boolean (match name with "<" -> order < 0 | "<=" -> order <= 0 | ">" -> order > 0 | _ -> order >= 0))
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
            | "length", [Items items] -> single (number (Rational.integer (bigint (List.length items))))
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
                match Map.tryFind alias world.Outputs with
                | Some previous when previous <> contents -> error "E_OUTPUT" $"Output '{alias}' already has different contents"
                | _ -> Ok [text contents, { world with Outputs = Map.add alias contents world.Outputs }]
            | "choose", [Scalar (Text key); Items alternatives] ->
                if key = "" then error "E_CHOICE" "A choice key must not be empty"
                else
                    distribution alternatives
                    |> Result.bind (fun domain ->
                        match Map.tryFind key world.Choices with
                        | Some decision when decision.Domain <> domain -> error "E_KEY" $"Choice '{key}' was reused with a different distribution"
                        | Some decision -> single (Value.ofData decision.Selected)
                        | None ->
                            domain |> List.map (fun (probability, value) ->
                                let next =
                                    { world with
                                        Weight = Rational.multiply world.Weight probability
                                        Choices = Map.add key { Domain = domain; Selected = value } world.Choices }
                                Value.ofData value, next) |> Ok)
            | _ -> error "E_TYPE" $"Invalid argument types for '{name}'"
        result |> Result.mapError (fun (code, message) -> Diagnostic.make code message span)

