module Mosaic.Tests

open Mosaic

let unwrap = function Ok value -> value | Error error -> failwithf "%A" error
let equal expected actual = if actual <> expected then failwithf "Expected %A; got %A" expected actual
let summary source = Engine.run source |> unwrap |> Engine.summary
let result source expected = fun () -> equal expected (summary source)
let error source code = fun () ->
    match Engine.run source with
    | Error diagnostic -> equal code diagnostic.Code
    | Ok report -> failwithf "Expected %s; got %s" code (Engine.summary report)

let tests = [
    "factorial / arbitrary precision", result "(letrec [f (fn [n] (if (= n 0) 1 (* n (f (- n 1)))))] (f 30))" "265252859812191058636308480000000"
    "integer division", result "(/ 7 3)" "2"
    "integer comparison", result "[(< 2 3) (<= 3 3) (> 4 5) (>= 5 5)]" "[true true false true]"
    "division truncates toward zero", result "[(/ -7 3) (/ 7 -3) (mod -7 3)]" "[-2 -2 -1]"
    "sequential let", result "(let [x 4 y (+ x 1)] (* x y))" "20"
    "lexical scope", result "(let [x 10 f (fn [y] (+ x y))] (let [x 99] (f 2)))" "12"
    "partial application", result "(let [add (fn [x y] (+ x y)) inc (add 1)] (inc 41))" "42"
    "over application", result "((fn [x] (fn [y] (+ x y))) 20 22)" "42"
    "builtin partial", result "(map (+ 10) [1 2 3])" "[11 12 13]"
    "mutual recursion", result "(letrec [even (fn [n] (if (= n 0) true (odd (- n 1)))) odd (fn [n] (if (= n 0) false (even (- n 1))))] [(even 10) (odd 11)])" "[true true]"
    "recursive lexical environment", result "(let [x 3] (letrec [f (fn [n] (if (= n 0) x (f (- n 1))))] (let [x 9] (f 4))))" "3"
    "recursive partial application", result "(letrec [sum (fn [n acc] (if (= n 0) acc (sum (- n 1) (+ acc n))))] ((sum 100) 0))" "5050"
    "unselected branch is lazy", result "(if true 42 (/ 1 0))" "42"
    "unforced suspension", result "(let [bomb (delay (/ 1 0))] 42)" "42"
    "force lexical scope", result "(let [x 3 pending (delay x)] (let [x 9] (force pending)))" "3"
    "infinite stream prefix", result "(stream-take 6 (iterate (fn [n] (* n 2)) 1))" "[1 2 4 8 16 32]"
    "stream does not over-force", result "(stream-take 1 [42 (delay (/ 1 0))])" "[42]"
    "list functions", result "(sum (filter (fn [x] (= (mod x 2) 0)) (map (fn [x] (* x x)) (range 1 6))))" "20"
    "reverse", result "(reverse [1 2 3])" "[3 2 1]"
    "zip shortest", result "(zip [1 2] [3])" "[[1 3]]"
    "higher order composition", result "((compose (+ 1) (* 2)) 20)" "41"
    "functional list equality", result "(= [1 [true]] [1 [true]])" "true"
    "comments and string escapes", result "; start\n(text-append \"line\\n\" \"a;[]\")" "\"line\\na;[]\""
    "unbound name", error "unknown" "E_NAME"
    "division by zero", error "(/ 2 0)" "E_ZERO"
    "empty head", error "(head [])" "E_EMPTY"
    "empty tail", error "(tail [])" "E_EMPTY"
    "strict argument", error "((fn [ignored] 42) (/ 1 0))" "E_ZERO"
    "Boolean condition required", error "(if 1 2 3)" "E_TYPE"
    "force type", error "(force 3)" "E_FORCE"
    "function final result", error "(fn [x] x)" "E_RESULT"
    "suspension final result", error "(delay 3)" "E_RESULT"
    "function equality", error "(= (fn [x] x) (fn [x] x))" "E_TYPE"
    "bad argument types", error "(+ 1 true)" "E_TYPE"
    "trailing expression", error "1 2" "E_TRAILING"
    "mismatched brackets", error "[1 2)" "E_DELIMITER"
    "missing bracket", error "(+ 1 2" "E_EOF"
    "unterminated string", error "\"oops" "E_STRING"
    "bad escape", error "\"a\\q\"" "E_ESCAPE"
    "duplicate parameters", error "(fn [x x] x)" "E_BINDING"
    "duplicate binding", error "(let [x 1 x 2] x)" "E_BINDING"
    "odd bindings", error "(let [x 1 y] x)" "E_BINDING"
    "letrec restricts values", error "(letrec [x 2] x)" "E_LETREC"
    "empty lambda parameters", error "(fn [] 42)" "E_FN"
    "file input explicit", error "(read-text \"absent\")" "E_INPUT"
    "output single assignment", error "[(write-text \"a\" \"one\") (write-text \"a\" \"two\")]" "E_OUTPUT"
    "file boundary", (fun () ->
        let report = Engine.evaluate { Inputs = Map.ofList ["source", "hello"] } "(write-text \"result\" (text-append (read-text \"source\") \"!\"))" |> unwrap
        equal "hello!" (Engine.resolveOutput "result" report |> unwrap))
    "missing output", (fun () ->
        let report = Engine.run "42" |> unwrap
        match Engine.resolveOutput "missing" report with Error _ -> () | _ -> failwith "Expected missing output")
    "recursion limit", error "(letrec [f (fn [x] (f x))] (f 0))" "E_RECURSION"
    "positions", (fun () ->
        match Engine.run "(let [x 1]\n  (+ x missing))" with
        | Error diagnostic -> equal (2, 8) (diagnostic.Span.Start.Line, diagnostic.Span.Start.Column)
        | _ -> failwith "Expected error")
    "pipeline order", result "(pipe 3 (+ 1) (* 2))" "8"
    "pipeline list processing", result "(pipe (range 1 11) (filter (fn [x] (= (mod x 2) 0))) (map (fn [x] (* x x))) sum)" "220"
    "pipeline empty stages", result "(pipe [1 2])" "[1 2]"
    "pipeline nested", result "(pipe (pipe 2 (+ 1)) (* 4))" "12"
    "pipeline lexical scope", result "(let [x 5 step (fn [n] (+ x n))] (let [x 99] (pipe 3 step)))" "8"
    "pipeline passes functions", result "(pipe (+ 3) (fn [f] (f 4)))" "7"
    "pipeline initial error first", error "(pipe (/ 1 0) missing)" "E_ZERO"
    "pipeline stages stop on error", error "(pipe 1 (fn [x] (/ x 0)) missing)" "E_ZERO"
    "pipeline nonfunction stage", error "(pipe 1 2)" "E_CALL"
    "pipeline wrong input type", error "(pipe true (+ 1))" "E_TYPE"
    "pipeline missing initial", error "(pipe)" "E_FORM"
    "pipeline stage position", (fun () ->
        match Engine.run "(pipe 1\n  2)" with
        | Error diagnostic -> equal (2, 3) (diagnostic.Span.Start.Line, diagnostic.Span.Start.Column)
        | _ -> failwith "Expected error")
    "pipeline retains outputs", (fun () ->
        let report = Engine.run "(pipe \"hello\" (write-text \"first\") (text-append \"prefix: \") (write-text \"second\"))" |> unwrap
        equal "hello" (Engine.resolveOutput "first" report |> unwrap)
        equal "prefix: hello" (Engine.resolveOutput "second" report |> unwrap))
    "identical output accepted", result "[(write-text \"a\" \"x\") (write-text \"a\" \"x\")]" "[\"x\" \"x\"]"
    "remainder by zero", error "(mod 3 0)" "E_ZERO"
    "data cannot be called", error "(1 2)" "E_CALL"
    "empty collections", result "[(map (+ 1) []) (filter (fn [x] true) []) (sum []) (product [])]" "[[] [] 0 1]"
    "list helpers", result "[(take 2 [4 5 6]) (take 0 [4]) (append [1] [2]) (length [1 2 3])]" "[[4 5] [] [1 2] 3]"
    "predicate helpers short circuit", result "[(all (fn [x] (if (= x 0) false (/ 1 0))) [0 1]) (any (fn [x] (if (= x 0) true (/ 1 0))) [0 1]) (all (fn [x] (/ 1 0)) [])]" "[false true true]"
    "source nesting limit", error (String.replicate 258 "[" + "0" + String.replicate 258 "]") "E_DEPTH"
]

[<EntryPoint>]
let main _ =
    let results = tests |> List.map (fun (name, run) ->
        try run (); printfn "PASS %s" name; true
        with ex -> eprintfn "FAIL %s: %s" name ex.Message; false)
    let passed = results |> List.filter id |> List.length
    printfn "%d/%d tests passed" passed results.Length
    if passed = results.Length then 0 else 1
