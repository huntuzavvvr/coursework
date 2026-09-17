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
    "factorial / arbitrary precision", result "(letrec [f (fn [n] (if (= n 0) 1 (* n (f (- n 1)))))] (f 30))" "265252859812191058636308480000000 @ 1"
    "exact fractions", result "(+ (/ 1 3) (/ 1 6))" "1/2 @ 1"
    "rational comparison", result "(< (/ 2 3) (/ 3 4))" "true @ 1"
    "negative denominator", result "(/ 2 -4)" "-1/2 @ 1"
    "sequential let", result "(let [x 4 y (+ x 1)] (* x y))" "20 @ 1"
    "lexical scope", result "(let [x 10 f (fn [y] (+ x y))] (let [x 99] (f 2)))" "12 @ 1"
    "partial application", result "(let [add (fn [x y] (+ x y)) inc (add 1)] (inc 41))" "42 @ 1"
    "over application", result "((fn [x] (fn [y] (+ x y))) 20 22)" "42 @ 1"
    "builtin partial", result "(map (+ 10) [1 2 3])" "[11 12 13] @ 1"
    "mutual recursion", result "(letrec [even (fn [n] (if (= n 0) true (odd (- n 1)))) odd (fn [n] (if (= n 0) false (even (- n 1))))] [(even 10) (odd 11)])" "[true true] @ 1"
    "recursive lexical environment", result "(let [x 3] (letrec [f (fn [n] (if (= n 0) x (f (- n 1))))] (let [x 9] (f 4))))" "3 @ 1"
    "recursive partial application", result "(letrec [sum (fn [n acc] (if (= n 0) acc (sum (- n 1) (+ acc n))))] ((sum 100) 0))" "5050 @ 1"
    "tail calls use explicit machine", result "(letrec [go (fn [n acc] (if (= n 0) acc (go (- n 1) (+ acc 1))))] (go 20000 0))" "20000 @ 1"
    "deep non-tail calls", result "(letrec [go (fn [n] (if (= n 0) 0 (+ 1 (go (- n 1)))))] (go 10000))" "10000 @ 1"
    "unselected branch is lazy", result "(if true 42 (/ 1 0))" "42 @ 1"
    "unforced suspension", result "(let [bomb (delay (/ 1 0))] 42)" "42 @ 1"
    "force lexical scope", result "(let [x 3 pending (delay x)] (let [x 9] (force pending)))" "3 @ 1"
    "infinite stream prefix", result "(stream-take 6 (iterate (fn [n] (* n 2)) 1))" "[1 2 4 8 16 32] @ 1"
    "stream does not over-force", result "(stream-take 1 [42 (delay (/ 1 0))])" "[42] @ 1"
    "list functions", result "(sum (filter (fn [x] (= (mod x 2) 0)) (map (fn [x] (* x x)) (range 1 6))))" "20 @ 1"
    "reverse", result "(reverse [1 2 3])" "[3 2 1] @ 1"
    "zip shortest", result "(zip [1 2] [3])" "[[1 3]] @ 1"
    "higher order composition", result "((compose (+ 1) (* 2)) 20)" "41 @ 1"
    "functional list equality", result "(= [1 [true]] [1 [true]])" "true @ 1"
    "comments and string escapes", result "; start\n(text-append \"line\\n\" \"a;[]\")" "\"line\\na;[]\" @ 1"
    "weighted choice", result "(choose \"coin\" [[1 \"H\"] [3 \"T\"]])" "\"H\" @ 1/4\n\"T\" @ 3/4"
    "correlated choice", result "(let [coin (fn [x] (choose \"coin\" [[1 0] [1 1]]))] [(coin 0) (coin 0)])" "[0 0] @ 1/2\n[1 1] @ 1/2"
    "independent keys", result "(+ (choose \"a\" [[1 0] [1 1]]) (choose \"b\" [[1 0] [1 1]]))" "0 @ 1/4\n1 @ 1/2\n2 @ 1/4"
    "conditioning", result "(let [a (choose \"a\" [[1 0] [1 1]]) b (choose \"b\" [[1 0] [1 1]])] (observe (> (+ a b) 0) a))" "0 @ 1/3\n1 @ 2/3"
    "duplicate outcomes merge", result "(choose \"x\" [[1 7] [3 7] [4 8]])" "7 @ 1/2\n8 @ 1/2"
    "equivalent domains correlate", result "[(choose \"x\" [[1 0] [1 1]]) (choose \"x\" [[2 1] [2 0]])]" "[0 0] @ 1/2\n[1 1] @ 1/2"
    "zero weight ignored", result "(choose \"x\" [[0 1] [2 2]])" "2 @ 1"
    "force retains correlation", result "(let [x (delay (choose \"x\" [[1 0] [1 1]]))] [(force x) (force x)])" "[0 0] @ 1/2\n[1 1] @ 1/2"
    "discard avoids evaluating body", result "(let [a (choose \"a\" [[1 true] [1 false]])] (observe a (if a 42 (/ 1 0))))" "42 @ 1"
    "empty domain", error "(choose \"x\" [])" "E_WEIGHT"
    "negative weight", error "(choose \"x\" [[-1 0] [2 1]])" "E_WEIGHT"
    "all-zero weights", error "(choose \"x\" [[0 0]])" "E_WEIGHT"
    "choice rejects closures", error "(choose \"x\" [[1 (fn [x] x)]])" "E_CHOICE"
    "conflicting domain", error "[(choose \"x\" [[1 0] [1 1]]) (choose \"x\" [[1 0]])]" "E_KEY"
    "impossible observation", error "(observe false 42)" "E_IMPOSSIBLE"
    "a branch failure is not rejected evidence", error "(/ 1 (choose \"x\" [[1 0] [1 1]]))" "E_ZERO"
    "unbound name", error "unknown" "E_NAME"
    "division by zero", error "(/ 2 0)" "E_ZERO"
    "empty head", error "(head [])" "E_EMPTY"
    "empty tail", error "(tail [])" "E_EMPTY"
    "strict argument", error "((fn [ignored] 42) (/ 1 0))" "E_ZERO"
    "Boolean condition required", error "(if 1 2 3)" "E_TYPE"
    "observe Boolean required", error "(observe 1 2)" "E_TYPE"
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
    "evidence retained", (fun () ->
        let report = Engine.run "(observe (= (choose \"x\" [[1 0] [3 1]]) 0) 42)" |> unwrap
        equal "1/4" (Rational.format report.Evidence)
        equal 1 report.Witnesses.Length)
    "file boundary", (fun () ->
        let report = Engine.evaluate Limits.standard { Inputs = Map.ofList ["source", "hello"] } "(write-text \"result\" (text-append (read-text \"source\") \"!\"))" |> unwrap
        equal "hello!" (Engine.resolveOutput "result" report |> unwrap))
    "ambiguous file boundary", (fun () ->
        let report = Engine.run "(write-text \"a\" (choose \"x\" [[1 \"one\"] [1 \"two\"]]))" |> unwrap
        match Engine.resolveOutput "a" report with Error _ -> () | _ -> failwith "Must reject ambiguous output")
    "step budget", (fun () ->
        match Engine.evaluate { Limits.standard with MaxSteps = 1000 } { Inputs = Map.empty } "(letrec [f (fn [x] (f x))] (f 0))" with
        | Error diagnostic -> equal "E_STEPS" diagnostic.Code
        | _ -> failwith "Expected limit")
    "world budget", (fun () ->
        match Engine.evaluate { Limits.standard with MaxWorlds = 1 } { Inputs = Map.empty } "(choose \"x\" [[1 0] [1 1]])" with
        | Error diagnostic -> equal "E_WORLDS" diagnostic.Code
        | _ -> failwith "Expected limit")
    "positions", (fun () ->
        match Engine.run "(let [x 1]\n  (+ x missing))" with
        | Error diagnostic -> equal (2, 8) (diagnostic.Span.Start.Line, diagnostic.Span.Start.Column)
        | _ -> failwith "Expected error")
    "rational algebra grid", (fun () ->
        [1..9] |> List.iter (fun x ->
            [1..9] |> List.iter (fun y ->
                let a = Rational.create (bigint x) (bigint y)
                equal Rational.one (Rational.divide a a)
                equal Rational.zero (Rational.subtract a a))))
    "probability mass grid", (fun () ->
        [1..7] |> List.iter (fun a ->
            [1..7] |> List.iter (fun b ->
                let report = Engine.run $"(choose \"test\" [[{a} 0] [{b} 1]])" |> unwrap
                equal Rational.one (report.Outcomes |> List.fold (fun acc outcome -> Rational.add acc outcome.Probability) Rational.zero)
                equal Rational.one report.Evidence)))
]

[<EntryPoint>]
let main _ =
    let results = tests |> List.map (fun (name, run) ->
        try run (); printfn "PASS %s" name; true
        with ex -> eprintfn "FAIL %s: %s" name ex.Message; false)
    let passed = results |> List.filter id |> List.length
    printfn "%d/%d tests passed" passed results.Length
    if passed = results.Length then 0 else 1
