namespace Mosaic

module Engine =
    let evaluate context source =
        Reader.parse source
        |> Result.bind Prelude.attach
        |> Result.bind (fun expression ->
            Interpreter.run context Primitives.environment expression
            |> Result.bind (fun (value, outputs) ->
                Value.toData value
                |> Result.mapError (fun message -> Diagnostic.make "E_RESULT" message expression.Span)
                |> Result.map (fun data -> { Value = data; Outputs = outputs })))

    let run source = evaluate { Inputs = Map.empty } source

    let resolveOutput alias report =
        match Map.tryFind alias report.Outputs with
        | Some contents -> Ok contents
        | None -> Error $"Program did not produce output '{alias}'"

    let summary report = Data.format report.Value
