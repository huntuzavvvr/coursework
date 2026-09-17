namespace Mosaic

/// A CEK-like abstract machine: evaluation stack and pending worlds are immutable data.
module Machine =
    [<NoEquality; NoComparison>]
    type private Frame =
        | Select of Expr * Expr * Map<string, Value>
        | Guard of Expr * Map<string, Value>
        | Binding of string * (string * Expr) list * Expr * Map<string, Value>
        | CollectList of Value list * Expr list * Map<string, Value>
        | Callee of Expr list * Map<string, Value>
        | Arguments of Value * Value list * Expr list * Map<string, Value>
        | ApplyRest of Value list
        | ForceValue
    [<NoEquality; NoComparison>]
    type private Control = Evaluate of Expr * Map<string, Value> | Return of Value
    [<NoEquality; NoComparison>]
    type private State = { Control: Control; Stack: (Frame * Span) list; World: World; Location: Span }
    [<NoEquality; NoComparison>]
    type private Transition = Continue of State list | Finished of Value * World | Rejected | Failed of Diagnostic

    let private recursiveEnvironment definitions captured =
        List.fold (fun env (name, _, _) -> Map.add name (RecursiveClosure (name, definitions, captured)) env) captured definitions

    let private returned value state = Continue [{ state with Control = Return value }]
    let private evaluate expression env state =
        Continue [{ state with Control = Evaluate (expression, env); Location = expression.Span }]
    let private push frame span expression env state =
        evaluate expression env { state with Stack = (frame, span) :: state.Stack }
    let private fail code message state = Failed (Diagnostic.make code message state.Location)

    let private startBindings bindings body env state =
        match bindings with
        | [] -> evaluate body env state
        | (name, rhs) :: rest -> push (Binding (name, rest, body, env)) rhs.Span rhs env state

    let private apply context functionValue arguments state =
        let enter parameters body captured =
            let count = min (List.length parameters) (List.length arguments)
            let bound = List.zip (List.take count parameters) (List.take count arguments)
            let env = List.fold (fun acc (name, value) -> Map.add name value acc) captured bound
            let parametersLeft = List.skip count parameters
            let argumentsLeft = List.skip count arguments
            match parametersLeft, argumentsLeft with
            | _ :: _, _ -> returned (Closure (parametersLeft, body, env)) state
            | [], [] -> evaluate body env state
            | [], rest -> push (ApplyRest rest) state.Location body env state
        match functionValue with
        | Closure (parameters, body, captured) -> enter parameters body captured
        | RecursiveClosure (name, definitions, captured) ->
            let _, parameters, body = List.find (fun (identifier, _, _) -> identifier = name) definitions
            enter parameters body (recursiveEnvironment definitions captured)
        | Primitive (name, supplied) ->
            let values = supplied @ arguments
            let arity = Map.find name Primitives.arities
            if List.length values < arity then returned (Primitive (name, values)) state
            else
                let used = List.take arity values
                let rest = List.skip arity values
                match Primitives.invoke context state.Location name used state.World with
                | Error diagnostic -> Failed diagnostic
                | Ok branches ->
                    branches |> List.map (fun (value, world) ->
                        { state with
                            Control = Return value
                            World = world
                            Stack = if List.isEmpty rest then state.Stack else (ApplyRest rest, state.Location) :: state.Stack })
                    |> Continue
        | _ -> fail "E_CALL" "Expected a function" state

    let private step context state =
        match state.Control with
        | Evaluate (expression, env) ->
            match expression.Node with
            | Literal data -> returned (Value.ofData data) state
            | Variable name ->
                match Map.tryFind name env with
                | Some value -> returned value state
                | None -> fail "E_NAME" $"Unknown name '{name}'" state
            | Lambda (parameters, body) -> returned (Closure (parameters, body, env)) state
            | Recursive (definitions, body) -> evaluate body (recursiveEnvironment definitions env) state
            | Bind (bindings, body) -> startBindings bindings body env state
            | Conditional (condition, yes, no) -> push (Select (yes, no, env)) expression.Span condition env state
            | Observe (condition, body) -> push (Guard (body, env)) expression.Span condition env state
            | Delay body -> returned (Suspended (body, env)) state
            | Force body -> push ForceValue expression.Span body env state
            | ListExpr [] -> returned (Items []) state
            | ListExpr (head :: tail) -> push (CollectList ([], tail, env)) expression.Span head env state
            | Apply (callee, arguments) -> push (Callee (arguments, env)) expression.Span callee env state
        | Return value ->
            match state.Stack with
            | [] -> Finished (value, state.World)
            | (frame, span) :: rest ->
                let next = { state with Stack = rest; Location = span }
                match frame with
                | Select (yes, no, env) ->
                    match value with
                    | Scalar (Boolean b) -> evaluate (if b then yes else no) env next
                    | _ -> fail "E_TYPE" "if requires a Boolean condition" next
                | Guard (body, env) ->
                    match value with
                    | Scalar (Boolean true) -> evaluate body env next
                    | Scalar (Boolean false) -> Rejected
                    | _ -> fail "E_TYPE" "observe requires a Boolean condition" next
                | Binding (name, remaining, body, env) -> startBindings remaining body (Map.add name value env) next
                | CollectList (reversed, [], _) -> returned (Items (List.rev (value :: reversed))) next
                | CollectList (reversed, head :: tail, env) -> push (CollectList (value :: reversed, tail, env)) span head env next
                | Callee ([], _) -> fail "E_CALL" "An application requires arguments" next
                | Callee (head :: tail, env) -> push (Arguments (value, [], tail, env)) span head env next
                | Arguments (fn, reversed, [], _) -> apply context fn (List.rev (value :: reversed)) next
                | Arguments (fn, reversed, head :: tail, env) -> push (Arguments (fn, value :: reversed, tail, env)) span head env next
                | ApplyRest values -> apply context value values next
                | ForceValue ->
                    match value with
                    | Suspended (body, captured) -> evaluate body captured next
                    | _ -> fail "E_FORCE" "force expects a delayed expression" next

    let run limits context environment expression =
        let initial =
            { Control = Evaluate (expression, environment); Stack = []; Location = expression.Span
              World = { Weight = Rational.one; Choices = Map.empty; Outputs = Map.empty } }
        let finish witnesses steps created =
            let evidence = witnesses |> List.fold (fun acc witness -> Rational.add acc witness.PriorWeight) Rational.zero
            if evidence = Rational.zero then Error (Diagnostic.make "E_IMPOSSIBLE" "No world satisfies the observations" expression.Span)
            else
                let outcomes =
                    witnesses
                    |> List.fold (fun grouped witness ->
                        let weight = Map.tryFind witness.Value grouped |> Option.defaultValue Rational.zero
                        Map.add witness.Value (Rational.add weight witness.PriorWeight) grouped) Map.empty
                    |> Map.toList
                    |> List.map (fun (value, weight) -> { Value = value; Probability = Rational.divide weight evidence })
                Ok { Outcomes = outcomes; Evidence = evidence; Witnesses = List.rev witnesses; Steps = steps; WorldsCreated = created }
        let rec drive pending witnesses steps created =
            match pending with
            | [] -> finish witnesses steps created
            | state :: _ when steps >= limits.MaxSteps -> Error (Diagnostic.make "E_STEPS" "Evaluation step limit exceeded" state.Location)
            | state :: rest ->
                match step context state with
                | Failed diagnostic -> Error diagnostic
                | Rejected -> drive rest witnesses (steps + 1) created
                | Finished (value, world) ->
                    match Value.toData value with
                    | Error message -> Error (Diagnostic.make "E_RESULT" message state.Location)
                    | Ok data ->
                        let witness = { Value = data; PriorWeight = world.Weight; Choices = world.Choices; Outputs = world.Outputs }
                        drive rest (witness :: witnesses) (steps + 1) created
                | Continue states ->
                    let total = created + max 0 (List.length states - 1)
                    if total > limits.MaxWorlds then Error (Diagnostic.make "E_WORLDS" "World creation limit exceeded" state.Location)
                    else drive (states @ rest) witnesses (steps + 1) total
        if limits.MaxSteps <= 0 || limits.MaxWorlds <= 0 then
            Error (Diagnostic.make "E_LIMIT" "Limits must be positive" expression.Span)
        else drive [initial] [] 0 1
