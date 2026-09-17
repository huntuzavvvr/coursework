# Coursework · Mosaic

Курсовой проект: **Mosaic**, функциональный язык конечных возможных миров.
Реализация на F#, точные вероятности, коррелированные события, наблюдения,
ленивые потоки и объяснения результатов.

Весь самостоятельный проект находится в [mosaic/](mosaic/README.md).

```sh
cd mosaic
./mosaic run examples/detective.mos --explain
./scripts/verify.sh
```

Нужны .NET SDK 10 и Python 3 для сквозных тестов. Подробности установки,
языка и устройства интерпретатора — в README проекта.

- [Требования и их покрытие](mosaic/docs/requirements.md)
- [Руководство языка](mosaic/docs/language.md)
- [Семантика и архитектура](mosaic/docs/semantics.md)
- [Подготовка к защите](mosaic/docs/defense.md)
- [Использование ИИ](mosaic/AI_USAGE.md)
