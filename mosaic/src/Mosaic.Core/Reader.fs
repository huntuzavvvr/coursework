namespace Mosaic

open System
open System.Globalization
open System.Numerics

/// Immutable cursor and recursive descent. No parser-global position or buffers.
module Reader =
    type private Form = { Shape: Shape; Span: Span }
    and private Shape = Atom of string | StringAtom of string | Round of Form list | Square of Form list
    type private Cursor = { Source: string; Position: Position }
    exception private ReadFailure of Diagnostic

    let private fail code message span = raise (ReadFailure (Diagnostic.make code message span))
    let private at cursor = { Start = cursor.Position; Finish = cursor.Position }
    let private peek cursor =
        if cursor.Position.Offset < cursor.Source.Length then Some cursor.Source[cursor.Position.Offset] else None
    let private advance cursor =
        match peek cursor with
        | Some '\n' -> { cursor with Position = { Offset = cursor.Position.Offset + 1; Line = cursor.Position.Line + 1; Column = 1 } }
        | Some _ -> { cursor with Position = { cursor.Position with Offset = cursor.Position.Offset + 1; Column = cursor.Position.Column + 1 } }
        | None -> cursor
    let private charsToString chars = chars |> List.rev |> List.toArray |> String
    let rec private skipComment cursor =
        match peek cursor with
        | None | Some '\n' -> cursor
        | _ -> skipComment (advance cursor)
    let rec private skip cursor =
        match peek cursor with
        | Some c when Char.IsWhiteSpace c -> skip (advance cursor)
        | Some ';' -> skip (skipComment (advance cursor))
        | _ -> cursor
    let private delimiter c = Char.IsWhiteSpace c || List.contains c ['('; ')'; '['; ']'; ';'; '"']
    let rec private atom chars cursor =
        match peek cursor with
        | Some c when not (delimiter c) -> atom (c :: chars) (advance cursor)
        | _ -> charsToString chars, cursor
    let rec private quoted start chars cursor =
        match peek cursor with
        | None -> fail "E_STRING" "Unterminated string" { Start = start; Finish = cursor.Position }
        | Some '"' -> charsToString chars, advance cursor
        | Some '\\' ->
            let next = advance cursor
            let decoded =
                match peek next with
                | Some 'n' -> '\n'
                | Some 'r' -> '\r'
                | Some 't' -> '\t'
                | Some '"' -> '"'
                | Some '\\' -> '\\'
                | _ -> fail "E_ESCAPE" "Supported escapes: \\n, \\r, \\t, \\\\, \\\"" (at next)
            quoted start (decoded :: chars) (advance next)
        | Some c -> quoted start (c :: chars) (advance cursor)

    let rec private read depth cursor =
        let current = skip cursor
        if depth > 256 then fail "E_DEPTH" "Source nesting exceeds 256 levels" (at current)
        let start = current.Position
        let shape, rest =
            match peek current with
            | None -> fail "E_EOF" "Expected an expression" (at current)
            | Some '(' -> let items, tail = group (depth + 1) ')' [] (advance current) in Round items, tail
            | Some '[' -> let items, tail = group (depth + 1) ']' [] (advance current) in Square items, tail
            | Some '"' -> let text, tail = quoted start [] (advance current) in StringAtom text, tail
            | Some ')' | Some ']' -> fail "E_DELIMITER" "Unexpected closing delimiter" (at current)
            | _ -> let text, tail = atom [] current in Atom text, tail
        { Shape = shape; Span = { Start = start; Finish = rest.Position } }, rest
    and private group depth closing reversed cursor =
        let current = skip cursor
        match peek current with
        | None -> fail "E_EOF" $"Expected '{closing}'" (at current)
        | Some c when c = closing -> List.rev reversed, advance current
        | Some ')' | Some ']' -> fail "E_DELIMITER" $"Expected '{closing}'" (at current)
        | _ -> let item, rest = read depth current in group depth closing (item :: reversed) rest

    let private reserved = Set.ofList ["let"; "letrec"; "fn"; "if"; "delay"; "force"; "observe"; "true"; "false"]
    let private numeric (text: string) =
        match BigInteger.TryParse(text, NumberStyles.AllowLeadingSign, CultureInfo.InvariantCulture) with
        | true, n -> Some n
        | _ -> None
    let private name form =
        match form.Shape with
        | Atom text when not (Set.contains text reserved) && Option.isNone (numeric text) -> text
        | _ -> fail "E_BINDING" "Expected a non-reserved identifier" form.Span
    let private unique names span =
        if Set.count (Set.ofList names) <> List.length names then fail "E_BINDING" "Duplicate binding or parameter" span
    let private parameters form =
        match form.Shape with
        | Square items when not (List.isEmpty items) ->
            let names = List.map name items
            unique names form.Span
            names
        | _ -> fail "E_FN" "Expected a nonempty parameter list: (fn [x y] body)" form.Span

    let rec private lower form =
        let node =
            match form.Shape with
            | StringAtom s -> Literal (Text s)
            | Atom "true" -> Literal (Data.Boolean true)
            | Atom "false" -> Literal (Data.Boolean false)
            | Atom text ->
                match numeric text with
                | Some n -> Literal (Number (Rational.integer n))
                | None -> Variable text
            | Square forms -> ListExpr (List.map lower forms)
            | Round ({ Shape = Atom "fn" } :: [args; body]) -> Lambda (parameters args, lower body)
            | Round ({ Shape = Atom "if" } :: [condition; yes; no]) -> Conditional (lower condition, lower yes, lower no)
            | Round ({ Shape = Atom "let" } :: [bindings; body]) -> Bind (readBindings bindings, lower body)
            | Round ({ Shape = Atom "letrec" } :: [bindings; body]) ->
                let functions = readBindings bindings |> List.map (fun (identifier, expression) ->
                    match expression.Node with
                    | Lambda (args, rhs) -> identifier, args, rhs
                    | _ -> fail "E_LETREC" "letrec accepts only function definitions" expression.Span)
                Recursive (functions, lower body)
            | Round ({ Shape = Atom "delay" } :: [body]) -> Delay (lower body)
            | Round ({ Shape = Atom "force" } :: [body]) -> Force (lower body)
            | Round ({ Shape = Atom "observe" } :: [condition; body]) -> Observe (lower condition, lower body)
            | Round ({ Shape = Atom keyword } :: _) when Set.contains keyword reserved ->
                fail "E_FORM" $"Invalid '{keyword}' form" form.Span
            | Round (callee :: (_ :: _ as arguments)) -> Apply (lower callee, List.map lower arguments)
            | Round _ -> fail "E_CALL" "An application requires a function and at least one argument" form.Span
        { Node = node; Span = form.Span }
    and private readBindings form =
        let rec pairs = function
            | [] -> []
            | identifier :: expression :: rest -> (name identifier, lower expression) :: pairs rest
            | _ -> fail "E_BINDING" "Bindings must alternate names and expressions" form.Span
        match form.Shape with
        | Square items ->
            let bindings = pairs items
            unique (List.map fst bindings) form.Span
            bindings
        | _ -> fail "E_BINDING" "Expected [name expression ...]" form.Span

    let parse source =
        try
            let form, rest = read 0 { Source = source; Position = Diagnostic.origin }
            let remaining = skip rest
            if Option.isSome (peek remaining) then fail "E_TRAILING" "Expected exactly one top-level expression" (at remaining)
            Ok (lower form)
        with ReadFailure error -> Error error
