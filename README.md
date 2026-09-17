# Mosaic — функциональный язык программирования

Индивидуальный курсовой проект на F#. Реализован небольшой интерпретатор
с функциями, замыканиями, рекурсией, списками и ленивыми вычислениями.
Особенность языка — связанные вероятностные выборы: повторное обращение
к одному событию сохраняет выбранное значение.

## Содержание

- [Проект и руководство по запуску](mosaic/README.md)
- [Синтаксис](mosaic/docs/syntax.md)
- [Стандартная библиотека](mosaic/docs/api.md)
- [Примеры программ](mosaic/docs/examples.md)
- [Архитектура](mosaic/docs/architecture.md)
- [Статус и требования](mosaic/PROJECT_STATUS.md)
- [Использование ИИ](mosaic/AI_USAGE.md)

## Пример

```lisp
(letrec [fact (fn [n]
               (if (= n 0) 1 (* n (fact (- n 1)))))]
  (fact 6))
```

Результат: `720 @ 1`.

## Запуск

Нужен .NET SDK 10. Из корня репозитория:

```sh
cd mosaic
./mosaic examples/factorial.mos
./mosaic examples/shared-weather.mos --explain
./scripts/verify.sh
```

Полная документация находится в папке [mosaic/](mosaic/README.md).

## Автор

[@huntuzavvvr](https://github.com/huntuzavvvr), индивидуальная работа.
Использование OpenAI Codex описано в [журнале](mosaic/AI_USAGE.md).
