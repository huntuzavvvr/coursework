# Mosaic — функциональный язык для обработки списков

Индивидуальный курсовой проект на F#. Язык поддерживает функции, замыкания,
рекурсию, списки и отложенные вычисления. Выражение `pipe` позволяет записать
обработку данных как последовательность шагов.

```lisp
(pipe (range 1 11)
  (filter (fn [x] (= (mod x 2) 0)))
  (map (fn [x] (* x x)))
  sum)
```

Результат: `220`. Программа выбирает чётные числа от 1 до 10, возводит их
в квадрат и складывает.

## Запуск

Нужен .NET SDK 10. Из корня репозитория:

```sh
cd mosaic
./mosaic examples/pipeline.mos
./scripts/verify.sh
```

Для сквозных проверок нужен Python 3.10+.

## Содержание

- [Проект и руководство по запуску](mosaic/README.md)
- [Синтаксис](mosaic/docs/syntax.md)
- [Стандартная библиотека](mosaic/docs/api.md)
- [Примеры программ](mosaic/docs/examples.md)
- [Архитектура](mosaic/docs/architecture.md)
- [Статус проекта](mosaic/PROJECT_STATUS.md)
- [Использование ИИ](mosaic/AI_USAGE.md)

## Автор

[@huntuzavvvr](https://github.com/huntuzavvvr), индивидуальная работа.
Использование OpenAI Codex описано в [журнале](mosaic/AI_USAGE.md).
