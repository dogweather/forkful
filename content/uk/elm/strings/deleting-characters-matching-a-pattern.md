---
date: 2024-01-20 17:42:23.594076-07:00
description: "How to: / \u042F\u043A \u0446\u0435 \u0437\u0440\u043E\u0431\u0438\u0442\
  \u0438: \u041F\u0440\u043E\u0441\u0442\u043E \u0441\u043A\u043E\u043F\u0456\u044E\
  \u0439\u0442\u0435 \u0446\u0435\u0439 \u043A\u043E\u0434 \u0432 \u0432\u0430\u0448\
  \ Elm \u043F\u0440\u043E\u0454\u043A\u0442, \u0442\u0430 \u0432\u0456\u043D \u0432\
  \u0438\u0434\u0430\u043B\u0438\u0442\u044C \u0443\u0441\u0435, \u0449\u043E \u043D\
  \u0435 \u0454 \u043B\u0456\u0442\u0435\u0440\u0430\u043C\u0438, \u0446\u0438\u0444\
  \u0440\u0430\u043C\u0438 \u0447\u0438 \u043F\u0440\u043E\u0431\u0456\u043B\u0430\
  \u043C\u0438."
lastmod: '2024-04-05T22:38:48.195247-06:00'
model: gpt-4-1106-preview
summary: "/ \u042F\u043A \u0446\u0435 \u0437\u0440\u043E\u0431\u0438\u0442\u0438:\
  \ \u041F\u0440\u043E\u0441\u0442\u043E \u0441\u043A\u043E\u043F\u0456\u044E\u0439\
  \u0442\u0435 \u0446\u0435\u0439 \u043A\u043E\u0434 \u0432 \u0432\u0430\u0448 Elm\
  \ \u043F\u0440\u043E\u0454\u043A\u0442, \u0442\u0430 \u0432\u0456\u043D \u0432\u0438\
  \u0434\u0430\u043B\u0438\u0442\u044C \u0443\u0441\u0435, \u0449\u043E \u043D\u0435\
  \ \u0454 \u043B\u0456\u0442\u0435\u0440\u0430\u043C\u0438, \u0446\u0438\u0444\u0440\
  \u0430\u043C\u0438 \u0447\u0438 \u043F\u0440\u043E\u0431\u0456\u043B\u0430\u043C\
  \u0438."
title: "\u0412\u0438\u0434\u0430\u043B\u0435\u043D\u043D\u044F \u0441\u0438\u043C\u0432\
  \u043E\u043B\u0456\u0432 \u0437\u0430 \u0432\u0456\u0437\u0435\u0440\u0443\u043D\
  \u043A\u043E\u043C"
weight: 5
---

## How to: / Як це зробити:
```Elm
module Main exposing (..)
import Regex exposing (Regex, find, regex, replace)
import String

cleanText : String -> String
cleanText text =
    let
        pattern : Regex
        pattern =
            regex "[^a-zA-Zа-яА-Я0-9 ]"
    in
    replace pattern (\_ -> "") text

main =
    String.split "\n" "123abcАБВ!@#\nHello+*/-World\nДоброго%%^&&дня"
    |> List.map cleanText
    |> String.join "\n"
    |> Html.text
    
-- Output:
-- 123abcАБВ
-- HelloWorld
-- Доброгодня
```
Просто скопіюйте цей код в ваш Elm проєкт, та він видалить усе, що не є літерами, цифрами чи пробілами.

## Deep Dive / Глибоке Занурення:
У видаленні символів за паттерном часто використовують регулярні вирази (regex). Elm використовує бібліотеку `elm/regex` для роботи з ними. Рідше, можна використовувати `String` функції без regex, але це менш гнучко. Регулярні вирази мають історію з 1950-х, і сьогодні є стандартом для операцій з текстом.

## See Also / Дивіться Також:
- Elm Regex package documentation: [https://package.elm-lang.org/packages/elm/regex/latest](https://package.elm-lang.org/packages/elm/regex/latest)
- Elm String module documentation: [https://package.elm-lang.org/packages/elm/core/latest/String](https://package.elm-lang.org/packages/elm/core/latest/String)
- Regular expressions in-depth: [https://developer.mozilla.org/en-US/docs/Web/JavaScript/Guide/Regular_Expressions](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Guide/Regular_Expressions)
