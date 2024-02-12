---
title:                "Пошук та заміна тексту"
aliases:
- /uk/elm/searching-and-replacing-text.md
date:                  2024-01-20T17:57:39.342171-07:00
model:                 gpt-4-1106-preview
simple_title:         "Пошук та заміна тексту"

tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/uk/elm/searching-and-replacing-text.md"
---

{{< edit_this_page >}}

## Що і Навіщо?
Пошук та заміна тексту - це замінення одного набору символів іншим у рядку. Програмісти використовують це для виправлення помилок, оновлення даних та рефакторингу коду.

## Як це зробити:
```Elm
module Main exposing (..)

import Browser
import Html exposing (text)
import Regex exposing (..)

main =
    let
        originalText = "Hello, Elm!"
        searchPattern = regex "Elm"
        replacementText = "World"
        newText = replace All searchPattern (\_ -> replacementText) originalText
    in
    Html.beginnerProgram { model = newText, view = view, update = update }

view model =
    text model

update msg model =
    model

-- Вивід буде: "Hello, World!"
```

## Поглиблений Розділ:
Пошук і заміна тексту в Elm здійснюється через модуль `Regex`. Його інтерфейс схожий на регулярні вирази в більшості мов програмування, але з Elm-специфічним API. В історичному контексті, робота з регулярними виразами була і є основою обробки текстів у програмуванні, починаючи з UNIX утиліти `sed`. Альтернативи в Elm, наприклад, включають функції для роботи зі списками та рядками без регулярних виразів, але вони менш гнучкі. Деталі реалізації включають різні режими підстановки (наприклад, `All` для заміни всіх входжень) та callback функції для динамічної заміни.

## Дивіться Також:
- Офіційна документація по Elm: [Official Elm Guide](https://guide.elm-lang.org/)
- Документація модуля `Regex` в Elm: [Elm Regex Documentation](https://package.elm-lang.org/packages/elm/regex/latest/)
- Інтерактивний пісочниця для тестування регулярних виразів: [Regex101](https://regex101.com/)
