namespace Mosaic

/// An expression returns values together with the worlds that produced them.
module Interpreter =
    let maxDepth = 256
    let maxWorlds = 1024
    exception private EvaluationError of Diagnostic

    let private fail span code message =
        raise (EvaluationError (Diagnostic.make code message span))

    // List.collect with a check while the resulting branches are combined.
    let private collect span branches next =
        let rec loop count reversed = function
            | [] -> List.rev reversed
            | branch :: rest ->
                let results = next branch
                let total = count + List.length results
                if total > maxWorlds then fail span "E_WORLDS" "More than 1024 intermediate worlds"
                loop total (List.fold (fun acc item -> item :: acc) reversed results) rest
        loop 0 [] branches

    let private recursiveEnvironment definitions captured =
        definitions |> List.fold (fun env (name, _, _) ->
            Map.add name (RecursiveClosure (name, definitions, captured)) env) captured

    let run context environment expression =
        let rec eval depth env world expr =
            if depth > maxDepth then fail expr.Span "E_RECURSION" "Evaluation nesting exceeds 256 levels"
            let child = eval (depth + 1)
            match expr.Node with
            | Literal data -> [Value.ofData data, world]
            | Variable name ->
                match Map.tryFind name env with
                | Some value -> [value, world]
                | None -> fail expr.Span "E_NAME" $"Unknown name '{name}'"
            | Lambda (parameters, body) -> [Closure (parameters, body, env), world]
            | Recursive (definitions, body) -> child (recursiveEnvironment definitions env) world body
            | Bind (bindings, body) ->
                let environments =
                    bindings |> List.fold (fun branches (name, rhs) ->
                        collect expr.Span branches (fun (scope, current) ->
                            child scope current rhs |> List.map (fun (value, next) -> Map.add name value scope, next))) [env, world]
                collect expr.Span environments (fun (scope, current) -> child scope current body)
            | Conditional (condition, yes, no) ->
                collect expr.Span (child env world condition) (fun (value, current) ->
                    match value with
                    | Scalar (Boolean answer) -> child env current (if answer then yes else no)
                    | _ -> fail expr.Span "E_TYPE" "if requires a Boolean condition")
            | Observe (condition, body) ->
                collect expr.Span (child env world condition) (fun (value, current) ->
                    match value with
                    | Scalar (Boolean true) -> child env current body
                    | Scalar (Boolean false) -> []
                    | _ -> fail expr.Span "E_TYPE" "observe requires a Boolean condition")
            | Delay body -> [Suspended (body, env), world]
            | Force delayed ->
                collect expr.Span (child env world delayed) (fun (value, current) ->
                    match value with
                    | Suspended (body, captured) -> child captured current body
                    | _ -> fail expr.Span "E_FORCE" "force expects a delayed expression")
            | ListExpr items -> evalList depth expr.Span env world items |> List.map (fun (values, current) -> Items values, current)
            | Apply (callee, arguments) ->
                collect expr.Span (child env world callee) (fun (fn, current) ->
                    collect expr.Span (evalList depth expr.Span env current arguments) (fun (values, next) ->
                        apply depth expr.Span fn values next))

        and evalList depth span env world expressions =
            expressions
            |> List.fold (fun branches item ->
                collect span branches (fun (reversed, current) ->
                    eval (depth + 1) env current item |> List.map (fun (value, next) -> value :: reversed, next))) [[], world]
            |> List.map (fun (reversed, current) -> List.rev reversed, current)

        and apply depth span fn arguments world =
            let applyRemaining results remaining =
                if List.isEmpty remaining then results
                else collect span results (fun (value, current) -> apply (depth + 1) span value remaining current)
            let enter parameters body captured =
                let count = min (List.length parameters) (List.length arguments)
                let bindings = List.zip (List.take count parameters) (List.take count arguments)
                let scope = bindings |> List.fold (fun env (name, value) -> Map.add name value env) captured
                match List.skip count parameters with
                | [] -> applyRemaining (eval (depth + 1) scope world body) (List.skip count arguments)
                | remaining -> [Closure (remaining, body, scope), world]
            match fn with
            | Closure (parameters, body, captured) -> enter parameters body captured
            | RecursiveClosure (name, definitions, captured) ->
                let _, parameters, body = List.find (fun (identifier, _, _) -> identifier = name) definitions
                enter parameters body (recursiveEnvironment definitions captured)
            | Primitive (name, supplied) ->
                let values = supplied @ arguments
                let arity = Map.find name Primitives.arities
                if List.length values < arity then [Primitive (name, values), world]
                else
                    match Primitives.invoke context span name (List.take arity values) world with
                    | Error diagnostic -> raise (EvaluationError diagnostic)
                    | Ok results ->
                        if List.length results > maxWorlds then fail span "E_WORLDS" "More than 1024 intermediate worlds"
                        applyRemaining results (List.skip arity values)
            | _ -> fail span "E_CALL" "Expected a function"

        try
            let initial = { Weight = Rational.one; Choices = Map.empty; Outputs = Map.empty }
            Ok (eval 0 environment initial expression)
        with EvaluationError diagnostic -> Error diagnostic
