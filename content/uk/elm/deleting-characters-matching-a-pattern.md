---
title:                "Видалення символів за візерунком"
aliases:
- uk/elm/deleting-characters-matching-a-pattern.md
date:                  2024-01-20T17:42:23.594076-07:00
model:                 gpt-4-1106-preview
simple_title:         "Видалення символів за візерунком"

tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/uk/elm/deleting-characters-matching-a-pattern.md"
---

{{< edit_this_page >}}

## What & Why? / Що і Чому?

У програмуванні, видалення символів за патерном означає фільтрацію тексту, грунтуючись на визначених критеріях. Програмісти роблять це, щоб очистити дані, видаливши непотрібні символи чи форматування.

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
