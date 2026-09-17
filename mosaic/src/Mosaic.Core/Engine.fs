namespace Mosaic

module Engine =
    let evaluate limits context source =
        Reader.parse source
        |> Result.bind Prelude.attach
        |> Result.bind (Machine.run limits context Primitives.environment)

    let run source = evaluate Limits.standard { Inputs = Map.empty } source

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

