namespace Mosaic

/// The higher-order standard library is written in Mosaic itself.
module Prelude =
    let source = """
(letrec [
  identity (fn [x] x)
  compose (fn [f g x] (f (g x)))
  map (fn [f xs]
        (if (empty? xs) [] (cons (f (head xs)) (map f (tail xs)))))
  filter (fn [predicate xs]
           (if (empty? xs) []
             (if (predicate (head xs))
               (cons (head xs) (filter predicate (tail xs)))
               (filter predicate (tail xs)))))
  foldl (fn [f acc xs]
          (if (empty? xs) acc (foldl f (f acc (head xs)) (tail xs))))
  reverse (fn [xs] (foldl (fn [acc x] (cons x acc)) [] xs))
  range (fn [start stop]
          (if (>= start stop) [] (cons start (range (+ start 1) stop))))
  sum (fn [xs] (foldl + 0 xs))
  product (fn [xs] (foldl * 1 xs))
  take (fn [n xs]
         (if (<= n 0) []
           (if (empty? xs) [] (cons (head xs) (take (- n 1) (tail xs))))))
  all (fn [predicate xs]
        (if (empty? xs) true
          (if (predicate (head xs)) (all predicate (tail xs)) false)))
  any (fn [predicate xs]
        (if (empty? xs) false
          (if (predicate (head xs)) true (any predicate (tail xs)))))
  zip (fn [xs ys]
        (if (empty? xs) []
          (if (empty? ys) []
            (cons [(head xs) (head ys)] (zip (tail xs) (tail ys))))))
  iterate (fn [step seed] [seed (delay (iterate step (step seed)))])
  stream-take (fn [n stream]
                (if (<= n 0) []
                  (cons (head stream)
                    (if (= n 1) []
                      (stream-take (- n 1) (force (head (tail stream))))))))
] 0)
"""

    let attach expression =
        match Reader.parse source with
        | Ok { Node = Recursive (definitions, _) } -> Ok { expression with Node = Recursive (definitions, expression) }
        | Ok _ -> Error (Diagnostic.make "E_PRELUDE" "Invalid prelude structure" expression.Span)
        | Error diagnostic -> Error diagnostic

