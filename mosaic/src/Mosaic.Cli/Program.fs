module Mosaic.Cli

open System
open System.IO
open Mosaic

type Options = { Explain: bool; Inputs: (string * string) list; Outputs: (string * string) list }
let defaults = { Explain = false; Inputs = []; Outputs = [] }
let usage = """Mosaic — functional language with named choices

Usage: mosaic FILE [--explain] [--input NAME PATH] [--output NAME PATH]

  --explain          show the choices in each surviving world
  --input NAME PATH  supply a UTF-8 file as a named input
  --output NAME PATH export a named output if all worlds agree
"""

let rec options current = function
    | [] -> Ok current
    | "--explain" :: rest -> options { current with Explain = true } rest
    | "--input" :: name :: path :: rest ->
        if List.exists (fun (alias, _) -> alias = name) current.Inputs then Error $"Duplicate input '{name}'"
        else options { current with Inputs = current.Inputs @ [name, path] } rest
    | "--output" :: name :: path :: rest ->
        if List.exists (fun (alias, target) -> alias = name || Path.GetFullPath target = Path.GetFullPath path) current.Outputs then
            Error "Output aliases and paths must be unique"
        else options { current with Outputs = current.Outputs @ [name, path] } rest
    | argument :: _ -> Error $"Unknown or incomplete option '{argument}'"

let execute path settings =
    let context = { Inputs = settings.Inputs |> List.map (fun (name, file) -> name, File.ReadAllText file) |> Map.ofList }
    match Engine.evaluate context (File.ReadAllText path) with
    | Error diagnostic -> eprintfn "%s" (Diagnostic.format path diagnostic); 1
    | Ok report ->
        let artifacts =
            settings.Outputs |> List.fold (fun result (name, target) ->
                result |> Result.bind (fun files ->
                    Engine.resolveOutput name report |> Result.map (fun contents -> (target, contents) :: files))) (Ok [])
        match artifacts with
        | Error message -> eprintfn "E_EXPORT: %s" message; 1
        | Ok files ->
            files |> List.rev |> List.iter (fun (target, contents) -> File.WriteAllText(target, contents, Text.UTF8Encoding(false)))
            printfn "%s" (Engine.summary report)
            printfn "evidence = %s; worlds = %d" (Rational.format report.Evidence) report.Witnesses.Length
            if settings.Explain then
                report.Witnesses |> List.iter (fun witness ->
                    let choices = witness.Choices |> Map.toList |> List.map (fun (name, decision) -> $"{name}={Data.format decision.Selected}") |> String.concat ", "
                    printfn "  %s -> %s" choices (Data.format witness.Value))
            0

[<EntryPoint>]
let main args =
    try
        // Keep the old `run FILE` spelling usable for existing commands.
        let arguments = match Array.toList args with "run" :: rest -> rest | rest -> rest
        match arguments with
        | [] | ["--help"] -> printfn "%s" usage; 0
        | path :: rest ->
            match options defaults rest with
            | Error message -> eprintfn "E_USAGE: %s" message; 2
            | Ok settings -> execute path settings
    with
    | :? IOException as error -> eprintfn "E_IO: %s" error.Message; 1
    | :? UnauthorizedAccessException as error -> eprintfn "E_IO: %s" error.Message; 1
    | :? ArgumentException as error -> eprintfn "E_USAGE: %s" error.Message; 2
