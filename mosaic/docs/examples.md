# Примеры программ

[Документация](index.md) · [Синтаксис](syntax.md) · [API](api.md)

Команды выполняются из папки `mosaic/`.

## Факториал

[Исходный файл](../examples/factorial.mos) вычисляет `20!` через рекурсию:

```sh
./mosaic examples/factorial.mos
```

Результат: `2432902008176640000`. `letrec` позволяет функции вызвать саму себя,
а `bigint` хранит результат без переполнения фиксированного целого типа.

## Замыкания

[closures.mos](../examples/closures.mos) создаёт функцию `2 * x + 3`:

```lisp
(let [make-affine (fn [scale offset] (fn [x] (+ (* scale x) offset)))
      transform (make-affine 2 3)]
  (map transform [0 1 5]))
```

Результат: `[3 5 13]`. Внутренняя функция сохраняет значения `scale` и `offset`.

## Списки

[lists.mos](../examples/lists.mos) строит квадраты от 1 до 10, выбирает чётные,
считает сумму и разворачивает первые три квадрата.

```sh
./mosaic examples/lists.mos
```

Результат: `[[4 16 36 64 100] 220 [9 4 1]]`.

## Цепочка преобразований

[pipeline.mos](../examples/pipeline.mos) записывает вычисление суммы в виде шагов:

```lisp
(pipe (range 1 11)
  (filter (fn [x] (= (mod x 2) 0)))
  (map (fn [x] (* x x)))
  sum)
```

Результат: `220`. После фильтра остаётся `[2 4 6 8 10]`, после `map` —
`[4 16 36 64 100]`. Каждый следующий шаг принимает результат предыдущего.

## Обработка оценок

[grades.mos](../examples/grades.mos) оставляет оценки от 3 до 5, добавляет
один балл там, где оценка меньше 5, и считает среднее:

```lisp
(let [grades [2 5 3 4 2 5]
      adjusted (pipe grades
                 (filter (fn [grade] (>= grade 3)))
                 (map (fn [grade] (if (< grade 5) (+ grade 1) grade))))]
  [adjusted (/ (sum adjusted) (length adjusted))])
```

Результат: `[[5 4 5 5] 4]`. Деление целочисленное, поэтому среднее округляется
к нулю. Пример предполагает непустой список прошедших фильтрацию оценок.

## Ленивый поток

[streams.mos](../examples/streams.mos) задаёт поток Фибоначчи:

```lisp
(letrec [fibs (fn [a b] [a (delay (fibs b (+ a b)))])]
  (stream-take 12 (fibs 0 1)))
```

Результат: `[0 1 1 2 3 5 8 13 21 34 55 89]`. Хвост вычисляется только тогда,
когда `stream-take` запрашивает следующий элемент.

## Работа с файлами

[files.mos](../examples/files.mos) дописывает строку к входному тексту.

```sh
mkdir -p out
./mosaic examples/files.mos \
  --input message examples/message.txt \
  --output report out/report.txt
```

В `out/report.txt` появится содержимое `message.txt` и строка
`Processed by Mosaic.`. Программа возвращает полученный текст: в консоли он
отображается как строковое значение с кавычками и escape-последовательностями.

Все семь примеров проверяются командой `./scripts/verify.sh`.
