namespace Mosaic

module Engine =
    let private summarize span branches =
        let witnesses =
            branches |> List.fold (fun result (value, (world: World)) ->
                result |> Result.bind (fun accumulated ->
                    Value.toData value
                    |> Result.mapError (fun message -> Diagnostic.make "E_RESULT" message span)
                    |> Result.map (fun data ->
                        { Value = data; PriorWeight = world.Weight; Choices = world.Choices; Outputs = world.Outputs } :: accumulated))) (Ok [])
        witnesses |> Result.bind (fun reversed ->
            let evidence = reversed |> List.fold (fun acc witness -> Rational.add acc witness.PriorWeight) Rational.zero
            if evidence = Rational.zero then Error (Diagnostic.make "E_IMPOSSIBLE" "No world satisfies the observations" span)
            else
                let outcomes =
                    reversed
                    |> List.fold (fun grouped witness ->
                        let previous = Map.tryFind witness.Value grouped |> Option.defaultValue Rational.zero
                        Map.add witness.Value (Rational.add previous witness.PriorWeight) grouped) Map.empty
                    |> Map.toList
                    |> List.map (fun (value, weight) -> { Value = value; Probability = Rational.divide weight evidence })
                Ok { Outcomes = outcomes; Evidence = evidence; Witnesses = List.rev reversed })

    let evaluate context source =
        Reader.parse source
        |> Result.bind Prelude.attach
        |> Result.bind (fun expression ->
            Interpreter.run context Primitives.environment expression
            |> Result.bind (summarize expression.Span))

    let run source = evaluate { Inputs = Map.empty } source

    /// A host may export only artifacts identical across every surviving world.
    let resolveOutput alias report =
        let alternatives = report.Witnesses |> List.map (fun witness -> Map.tryFind alias witness.Outputs) |> List.distinct
        match alternatives with
        | [Some contents] -> Ok contents
        | [None] -> Error $"No world produced output '{alias}'"
        | _ -> Error $"Output '{alias}' differs between worlds or is absent in some worlds"

    let summary report =
        report.Outcomes
        |> List.map (fun outcome -> $"{Data.format outcome.Value} @ {Rational.format outcome.Probability}")
        |> String.concat "\n"
