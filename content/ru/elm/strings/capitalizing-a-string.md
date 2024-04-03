---
changelog:
- 2024-01-28, gpt-4-0125-preview, translated from English
date: 2024-01-28 23:55:54.497825-07:00
description: "\u041A\u0430\u043A: \u0412 Elm \u043D\u0435\u0442 \u0432\u0441\u0442\
  \u0440\u043E\u0435\u043D\u043D\u043E\u0439 \u0444\u0443\u043D\u043A\u0446\u0438\u0438\
  \ capitalize, \u043D\u043E \u0435\u0451 \u043C\u043E\u0436\u043D\u043E \u043B\u0435\
  \u0433\u043A\u043E \u0441\u043E\u0437\u0434\u0430\u0442\u044C."
lastmod: '2024-03-13T22:44:44.869416-06:00'
model: gpt-4-0125-preview
summary: "\u0412 Elm \u043D\u0435\u0442 \u0432\u0441\u0442\u0440\u043E\u0435\u043D\
  \u043D\u043E\u0439 \u0444\u0443\u043D\u043A\u0446\u0438\u0438 capitalize, \u043D\
  \u043E \u0435\u0451 \u043C\u043E\u0436\u043D\u043E \u043B\u0435\u0433\u043A\u043E\
  \ \u0441\u043E\u0437\u0434\u0430\u0442\u044C."
title: "\u041F\u0440\u0435\u043E\u0431\u0440\u0430\u0437\u043E\u0432\u0430\u043D\u0438\
  \u0435 \u0441\u0442\u0440\u043E\u043A\u0438 \u0432 \u0432\u0435\u0440\u0445\u043D\
  \u0438\u0439 \u0440\u0435\u0433\u0438\u0441\u0442\u0440"
weight: 2
---

## Как:
В Elm нет встроенной функции capitalize, но её можно легко создать:

```Elm
import String exposing (toUpper, toLower, left, dropLeft)

capitalize : String -> String
capitalize text =
    if String.isEmpty text then
        ""
    else
        toUpper (left 1 text) ++ toLower (dropLeft 1 text)

main =
    String.words "hello elm world" |> List.map capitalize |> String.join " "
    -- Вывод: "Hello Elm World"
```

## Подробный Разбор
Elm предпочитает ясность и не включает в основные библиотеки общие операции со строками, такие как `capitalize`. Традиционно вы либо создавали своё собственное решение, либо использовали стороннюю библиотеку для расширения возможностей работы со строками.

Основная библиотека `String` в Elm предоставляет `toUpper` и `toLower`, которые обрабатывают преобразование всей строки. Для приведения к заглавному виду берётся первый символ с помощью `left`, преобразуется в верхний регистр с помощью `toUpper`, а затем присоединяется к остатку строки, преобразованной в нижний регистр с помощью `toLower`. Оставшаяся часть строки извлекается с помощью `dropLeft`, что позволяет избежать воздействия на первый символ.

Хотя стандартные библиотеки Elm могут не иметь встроенной функции `capitalize`, такое решение гарантирует минималистичное и производительное ядро, оставляя такие конкретные утилиты для реализации пользователями или дополнительных пакетов.

К альтернативам относится использование полноценных пакетов для манипуляции со строками, таких как `elm-string-extra`, которые включают функцию `capitalize` среди прочих полезных операций со строками:

```Elm
import String.Extra exposing (capitalize)

main =
    String.words "hello elm world" |> List.map capitalize |> String.join " "
    -- Вывод: "Hello Elm World"
```

Стоит отметить, что подход Elm к строкам осознанно учитывает Unicode, что означает корректную обработку приведения к заглавному виду даже для языков с нелатинским алфавитом, несмотря на дополнительные сложности.

## Смотрите Также
- Документация Elm по строкам: https://package.elm-lang.org/packages/elm/core/latest/String
- Библиотека `elm-string-extra` на Elm packages: https://package.elm-lang.org/packages/elm-community/string-extra/latest/
- Стандарт Unicode для преобразований регистра: https://www.unicode.org/reports/tr21/
