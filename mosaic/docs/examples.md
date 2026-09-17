---
layout: default
title: Примеры
---

# Примеры программ

[Главная](index.md) · [Синтаксис](syntax.md) · [API](api.md) · [Архитектура](architecture.md)

Все файлы находятся в `examples/`. Команды выполняются из папки `mosaic/`.
Обозначение `значение @ вероятность` показывает результат и его вероятность.
Для обычной программы вероятность равна `1`.

## 1. Факториал

Файл: `factorial.mos`.

```lisp
(letrec [factorial (fn [n]
                    (if (= n 0) 1 (* n (factorial (- n 1)))))]
  (factorial 20))
```

Результат: `2432902008176640000 @ 1`.
Пример показывает рекурсивный вызов, условие и большие целые числа.

## 2. Замыкания

Файл: `closures.mos`.

```lisp
(let [make-affine (fn [scale offset] (fn [x] (+ (* scale x) offset)))
      fahrenheit (make-affine (/ 9 5) 32)]
  (map fahrenheit [0 10 20 100]))
```

Результат: `[32 50 68 212] @ 1`. Созданная функция хранит коэффициент и
смещение, заданные при её определении.

## 3. Обработка списков

Файл: `lists.mos`.

```lisp
(let [squares (map (fn [x] (* x x)) (range 1 11))
      even-squares (filter (fn [x] (= (mod x 2) 0)) squares)]
  [even-squares (sum even-squares) (reverse (take 3 squares))])
```

Результат: `[[4 16 36 64 100] 220 [9 4 1]] @ 1`.
`map` вычисляет квадраты, `filter` оставляет чётные, `sum` находит их сумму.

## 4. Ленивый поток Фибоначчи

Файл: `streams.mos`.

```lisp
(letrec [fibs (fn [a b] [a (delay (fibs b (+ a b)))])]
  (stream-take 12 (fibs 0 1)))
```

Результат: `[0 1 1 2 3 5 8 13 21 34 55 89] @ 1`.
Хвост хранится как задержка. Поэтому бесконечная последовательность не
вычисляется целиком; программа запрашивает только первые 12 элементов.

## 5. Связанные выборы

Файл: `shared-weather.mos`.

```lisp
(let [weather (fn [ignored] (choose "weather" [[3 "sun"] [1 "rain"]]))]
  [(weather 0) (weather 0)])
```

Результат:

```text
["rain" "rain"] @ 1/4
["sun" "sun"] @ 3/4
evidence = 1; worlds = 2
```

Одинаковое имя события связывает два обращения. Если использовать разные
имена, получится четыре независимых сочетания: их вероятности будут
`9/16`, `3/16`, `3/16` и `1/16`.

## 6. Две игральные кости

Файл: `dice.mos`.

```lisp
(let [die (fn [name] (choose name (map (fn [n] [1 n]) (range 1 7))))
      left (die "left")
      right (die "right")]
  (observe (= (+ left right) 7) [left right]))
```

Из 36 равновероятных пар подходят 6. Поэтому `evidence = 1/6`, а каждая
выжившая пара `[1 6]`, `[2 5]`, …, `[6 1]` имеет условную вероятность `1/6`.

## 7. Чтение и запись файлов

Файл: `files.mos`. Его выражение:

```text
(write-text "report" (text-append (read-text "message") "\nProcessed by Mosaic.\n"))
```

Для него нужно передать входной текст и путь выхода:

```sh
mkdir -p out
./mosaic examples/files.mos \
  --input message examples/message.txt \
  --output report out/report.txt
```

В файл `out/report.txt` попадёт исходный текст и отметка об обработке.
Без `--output` физический файл не создаётся.

## Запуск и проверка

```sh
./mosaic examples/factorial.mos
./mosaic examples/shared-weather.mos --explain
./mosaic examples/dice.mos
./scripts/verify.sh
```

Флаг `--explain` показывает решения в выживших сценариях. Все семь примеров
сверяются с ожидаемыми ответами в `scripts/integration.py`.
