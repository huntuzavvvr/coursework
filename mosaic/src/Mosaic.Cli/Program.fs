module Mosaic.Cli

open System
open System.IO
open System.Text.Json
open Mosaic

type Options = { Explain: bool; Json: bool; Inputs: (string * string) list; Outputs: (string * string) list; Limits: Limits }
let defaults = { Explain = false; Json = false; Inputs = []; Outputs = []; Limits = Limits.standard }
let usage = """Mosaic 0.1 — exact possible worlds

  mosaic run FILE [OPTIONS]      evaluate a .mos program
  mosaic eval SOURCE [OPTIONS]   evaluate an expression
  mosaic check FILE              validate syntax (not types or termination)
  mosaic repl                    one expression per line; :quit to exit

OPTIONS
  --explain                      show surviving worlds and named decisions
  --json                         machine-readable report (includes witnesses)
  --input NAME PATH              snapshot a UTF-8 text file as a logical input
  --output NAME PATH             export an agreed logical output as UTF-8
  --max-steps N                  total machine transitions (default 2000000)
  --max-worlds N                 created scenario budget (default 10000)
"""

let positive (text: string) =
    match Int32.TryParse text with
    | true, n when n > 0 -> Ok n
    | _ -> Error $"Expected a positive integer, got '{text}'"

let rec options current = function
    | [] -> Ok current
    | "--explain" :: rest -> options { current with Explain = true } rest
    | "--json" :: rest -> options { current with Json = true } rest
    | "--input" :: name :: path :: rest ->
        if List.exists (fun (alias, _) -> alias = name) current.Inputs then Error $"Duplicate input '{name}'"
        else options { current with Inputs = current.Inputs @ [name, path] } rest
    | "--output" :: name :: path :: rest ->
        if List.exists (fun (alias, target) -> alias = name || Path.GetFullPath target = Path.GetFullPath path) current.Outputs then
            Error "Output aliases and paths must be unique"
        else options { current with Outputs = current.Outputs @ [name, path] } rest
    | "--max-steps" :: value :: rest -> positive value |> Result.bind (fun n -> options { current with Limits = { current.Limits with MaxSteps = n } } rest)
    | "--max-worlds" :: value :: rest -> positive value |> Result.bind (fun n -> options { current with Limits = { current.Limits with MaxWorlds = n } } rest)
    | argument :: _ -> Error $"Unknown or incomplete option '{argument}'"

let json report =
    let payload =
        {| evidence = Rational.format report.Evidence
           steps = report.Steps
           worldsCreated = report.WorldsCreated
           outcomes = report.Outcomes |> List.map (fun outcome ->
               {| value = Data.format outcome.Value; probability = Rational.format outcome.Probability |}) |> List.toArray
           witnesses = report.Witnesses |> List.map (fun witness ->
               {| value = Data.format witness.Value
                  priorWeight = Rational.format witness.PriorWeight
                  posteriorWeight = Rational.format (Rational.divide witness.PriorWeight report.Evidence)
                  decisions = witness.Choices |> Map.toArray |> Array.map (fun (name, decision) ->
                      {| name = name; value = Data.format decision.Selected |})
                  outputs = witness.Outputs |> Map.toArray |> Array.map (fun (name, contents) -> {| name = name; contents = contents |}) |}) |> List.toArray |}
    JsonSerializer.Serialize(payload, JsonSerializerOptions(WriteIndented = true))

let printReport settings report =
    if settings.Json then printfn "%s" (json report)
    else
        printfn "%s" (Engine.summary report)
        printfn "evidence = %s; worlds = %d" (Rational.format report.Evidence) report.Witnesses.Length
        if settings.Explain then
            report.Witnesses |> List.iteri (fun index witness ->
                let decisions = witness.Choices |> Map.toList |> List.map (fun (name, choice) -> $"{name}={Data.format choice.Selected}") |> String.concat ", "
                printfn "  #%d prior=%s posterior=%s -> %s {%s}" (index + 1) (Rational.format witness.PriorWeight)
                    (Rational.format (Rational.divide witness.PriorWeight report.Evidence)) (Data.format witness.Value) decisions)

let execute sourceName source settings =
    let context = { Inputs = settings.Inputs |> List.map (fun (name, path) -> name, File.ReadAllText path) |> Map.ofList }
    match Engine.evaluate settings.Limits context source with
    | Error diagnostic -> eprintfn "%s" (Diagnostic.format sourceName diagnostic); 1
    | Ok report ->
        let artifacts =
            settings.Outputs
            |> List.fold (fun result (name, path) ->
                result |> Result.bind (fun collected -> Engine.resolveOutput name report |> Result.map (fun contents -> (path, contents) :: collected))) (Ok [])
        match artifacts with
        | Error message -> eprintfn "E_EXPORT: %s" message; 1
        | Ok files ->
            // All semantic checks finish before the host writes any artifact.
            files |> List.rev |> List.iter (fun (path, contents) -> File.WriteAllText(path, contents, Text.UTF8Encoding(false)))
            printReport settings report
            0

let rec repl () =
    Console.Write "mosaic> "
    match Console.ReadLine () with
    | null | ":quit" -> 0
    | line when String.IsNullOrWhiteSpace line -> repl ()
    | line -> execute "<repl>" line defaults |> ignore; repl ()

[<EntryPoint>]
let main args =
    try
        match Array.toList args with
        | [] | ["--help"] | ["help"] -> printfn "%s" usage; 0
        | ["--version"] -> printfn "Mosaic 0.1.0"; 0
        | ["repl"] -> repl ()
        | ["check"; path] ->
            match File.ReadAllText path |> Reader.parse with
            | Ok _ -> printfn "%s: syntax OK" path; 0
            | Error diagnostic -> eprintfn "%s" (Diagnostic.format path diagnostic); 1
        | ("run" | "eval" as command) :: input :: rest ->
            match options defaults rest with
            | Error message -> eprintfn "E_USAGE: %s" message; 2
            | Ok settings ->
                let name, source = if command = "run" then input, File.ReadAllText input else "<eval>", input
                execute name source settings
        | _ -> eprintfn "%s" usage; 2
    with
    | :? IOException as error -> eprintfn "E_IO: %s" error.Message; 1
    | :? UnauthorizedAccessException as error -> eprintfn "E_IO: %s" error.Message; 1
    | :? ArgumentException as error -> eprintfn "E_USAGE: %s" error.Message; 2
