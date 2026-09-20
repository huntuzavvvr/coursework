namespace Mosaic

/// Recursive evaluation with immutable environments and pending file outputs.
module Interpreter =
    let maxDepth = 256
    exception private EvaluationError of Diagnostic

    let private fail span code message =
        raise (EvaluationError (Diagnostic.make code message span))

    let private recursiveEnvironment definitions captured =
        definitions |> List.fold (fun env (name, _, _) ->
            Map.add name (RecursiveClosure (name, definitions, captured)) env) captured

    let run context environment expression =
        let rec eval depth env outputs expr =
            if depth > maxDepth then fail expr.Span "E_RECURSION" "Evaluation nesting exceeds 256 levels"
            let child = eval (depth + 1)
            match expr.Node with
            | Literal data -> Value.ofData data, outputs
            | Variable name ->
                match Map.tryFind name env with
                | Some value -> value, outputs
                | None -> fail expr.Span "E_NAME" $"Unknown name '{name}'"
            | Lambda (parameters, body) -> Closure (parameters, body, env), outputs
            | Recursive (definitions, body) -> child (recursiveEnvironment definitions env) outputs body
            | Bind (bindings, body) ->
                let scope, pending =
                    bindings |> List.fold (fun (scope, current) (name, rhs) ->
                        let value, next = child scope current rhs
                        Map.add name value scope, next) (env, outputs)
                child scope pending body
            | Conditional (condition, yes, no) ->
                let value, pending = child env outputs condition
                match value with
                | Scalar (Boolean answer) -> child env pending (if answer then yes else no)
                | _ -> fail expr.Span "E_TYPE" "if requires a Boolean condition"
            | Delay body -> Suspended (body, env), outputs
            | Force delayed ->
                let value, pending = child env outputs delayed
                match value with
                | Suspended (body, captured) -> child captured pending body
                | _ -> fail expr.Span "E_FORCE" "force expects a delayed expression"
            | ListExpr items ->
                let values, pending = evalList depth env outputs items
                Items values, pending
            | Apply (callee, arguments) ->
                let fn, pending = child env outputs callee
                let values, next = evalList depth env pending arguments
                apply depth expr.Span fn values next
            | Pipeline (initial, stages) ->
                stages |> List.fold (fun (value, pending) stage ->
                    let fn, next = child env pending stage
                    apply depth stage.Span fn [value] next) (child env outputs initial)

        and evalList depth env outputs expressions =
            let reversed, pending =
                expressions |> List.fold (fun (values, current) item ->
                    let value, next = eval (depth + 1) env current item
                    value :: values, next) ([], outputs)
            List.rev reversed, pending

        and apply depth span fn arguments outputs =
            let applyRemaining (value, pending) remaining =
                if List.isEmpty remaining then value, pending
                else apply (depth + 1) span value remaining pending
            let enter parameters body captured =
                let count = min (List.length parameters) (List.length arguments)
                let bindings = List.zip (List.take count parameters) (List.take count arguments)
                let scope = bindings |> List.fold (fun env (name, value) -> Map.add name value env) captured
                match List.skip count parameters with
                | [] -> applyRemaining (eval (depth + 1) scope outputs body) (List.skip count arguments)
                | remaining -> Closure (remaining, body, scope), outputs
            match fn with
            | Closure (parameters, body, captured) -> enter parameters body captured
            | RecursiveClosure (name, definitions, captured) ->
                let _, parameters, body = List.find (fun (identifier, _, _) -> identifier = name) definitions
                enter parameters body (recursiveEnvironment definitions captured)
            | Primitive (name, supplied) ->
                let values = supplied @ arguments
                let arity = Map.find name Primitives.arities
                if List.length values < arity then Primitive (name, values), outputs
                else
                    match Primitives.invoke context span name (List.take arity values) outputs with
                    | Error diagnostic -> raise (EvaluationError diagnostic)
                    | Ok result -> applyRemaining result (List.skip arity values)
            | _ -> fail span "E_CALL" "Expected a function"

        try Ok (eval 0 environment Map.empty expression)
        with EvaluationError diagnostic -> Error diagnostic
