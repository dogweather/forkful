---
changelog:
- 2024-01-29, gpt-4-0125-preview, translated from English
date: 2024-01-29 00:04:33.088325-07:00
description: "\u041A\u0430\u043A \u0438\u0441\u043F\u043E\u043B\u044C\u0437\u043E\u0432\
  \u0430\u0442\u044C: Elm \u043D\u0435 \u0438\u043C\u0435\u0435\u0442 \u0432\u0441\
  \u0442\u0440\u043E\u0435\u043D\u043D\u044B\u0445 \u0432\u043E\u0437\u043C\u043E\u0436\
  \u043D\u043E\u0441\u0442\u0435\u0439 \u0434\u043B\u044F \u0440\u0430\u0431\u043E\
  \u0442\u044B \u0441 \u0440\u0435\u0433\u0443\u043B\u044F\u0440\u043D\u044B\u043C\
  \u0438 \u0432\u044B\u0440\u0430\u0436\u0435\u043D\u0438\u044F\u043C\u0438, \u043D\
  \u043E \u0432\u044B \u043C\u043E\u0436\u0435\u0442\u0435 \u0438\u0441\u043F\u043E\
  \u043B\u044C\u0437\u043E\u0432\u0430\u0442\u044C \u043F\u0430\u043A\u0435\u0442\
  \ `elm/regex`. \u0412\u043E\u0442 \u043A\u0430\u043A\u2026"
lastmod: '2024-03-13T22:44:44.882673-06:00'
model: gpt-4-0125-preview
summary: "Elm \u043D\u0435 \u0438\u043C\u0435\u0435\u0442 \u0432\u0441\u0442\u0440\
  \u043E\u0435\u043D\u043D\u044B\u0445 \u0432\u043E\u0437\u043C\u043E\u0436\u043D\u043E\
  \u0441\u0442\u0435\u0439 \u0434\u043B\u044F \u0440\u0430\u0431\u043E\u0442\u044B\
  \ \u0441 \u0440\u0435\u0433\u0443\u043B\u044F\u0440\u043D\u044B\u043C\u0438 \u0432\
  \u044B\u0440\u0430\u0436\u0435\u043D\u0438\u044F\u043C\u0438, \u043D\u043E \u0432\
  \u044B \u043C\u043E\u0436\u0435\u0442\u0435 \u0438\u0441\u043F\u043E\u043B\u044C\
  \u0437\u043E\u0432\u0430\u0442\u044C \u043F\u0430\u043A\u0435\u0442 `elm/regex`."
title: "\u0418\u0441\u043F\u043E\u043B\u044C\u0437\u043E\u0432\u0430\u043D\u0438\u0435\
  \ \u0440\u0435\u0433\u0443\u043B\u044F\u0440\u043D\u044B\u0445 \u0432\u044B\u0440\
  \u0430\u0436\u0435\u043D\u0438\u0439"
weight: 11
---

## Как использовать:
Elm не имеет встроенных возможностей для работы с регулярными выражениями, но вы можете использовать пакет `elm/regex`. Вот как использовать regex для распространенных задач:

```Elm
import Regex exposing (..)

-- Примеры использования regex в Elm --

-- Проверка, содержит ли строка "hello"
checkForHello : String -> Bool
checkForHello input =
    let
        pattern = "hello"
        regex = Regex.fromString pattern |> Maybe.withDefault (regex ".")
    in
    Regex.contains regex input

-- Пример выходных данных
checkForHello "hello, world!" -- True

-- Извлечение цифр из строки
extractDigits : String -> List String
extractDigits input =
    let
        regex = Regex.fromString "\\d+" |> Maybe.withDefault (regex ".")
    in
    Regex.find (All) regex input |> List.map .match

-- Пример выходных данных
extractDigits "elm123rocks" -- ["123"]
```
Помните, что вам нужно обрабатывать Maybe для потенциальных неудач при сопоставлении шаблонов при использовании `Regex.fromString`.

## Глубже в тему
Регулярные выражения возвращаются к 1950-м годам, с корнями в теории автоматов и теории формальных языков. Со временем regex стали мощным инструментом в обработке текста, интегрированным во многие языки программирования и утилиты командной строки.

Альтернативы regex в Elm включают в себя функции для работы со строками, такие как `String.contains`, `String.startsWith`, `String.split` и т.д. Хотя они и проще, они менее мощные для сложного сопоставления с образцом.

С точки зрения реализации, regex в Elm построены на основе движка регулярных выражений JavaScript, благодаря среде выполнения Elm. Это означает, что поведение regex в Elm может отражать возможности и ограничения JavaScript.

## См. также
- Пакет Elm Regex: [package.elm-lang.org/packages/elm/regex/latest](https://package.elm-lang.org/packages/elm/regex/latest)
- Регулярные выражения в JavaScript: [developer.mozilla.org/en-US/docs/Web/JavaScript/Guide/Regular_Expressions](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Guide/Regular_Expressions)
- Инструменты для тестирования и отладки regex: [regex101.com](https://regex101.com)
