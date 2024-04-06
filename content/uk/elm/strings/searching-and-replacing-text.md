---
date: 2024-01-20 17:57:39.342171-07:00
description: "\u042F\u043A \u0446\u0435 \u0437\u0440\u043E\u0431\u0438\u0442\u0438\
  : \u041F\u043E\u0448\u0443\u043A \u0456 \u0437\u0430\u043C\u0456\u043D\u0430 \u0442\
  \u0435\u043A\u0441\u0442\u0443 \u0432 Elm \u0437\u0434\u0456\u0439\u0441\u043D\u044E\
  \u0454\u0442\u044C\u0441\u044F \u0447\u0435\u0440\u0435\u0437 \u043C\u043E\u0434\
  \u0443\u043B\u044C `Regex`. \u0419\u043E\u0433\u043E \u0456\u043D\u0442\u0435\u0440\
  \u0444\u0435\u0439\u0441 \u0441\u0445\u043E\u0436\u0438\u0439 \u043D\u0430 \u0440\
  \u0435\u0433\u0443\u043B\u044F\u0440\u043D\u0456 \u0432\u0438\u0440\u0430\u0437\u0438\
  \ \u0432 \u0431\u0456\u043B\u044C\u0448\u043E\u0441\u0442\u0456 \u043C\u043E\u0432\
  \ \u043F\u0440\u043E\u0433\u0440\u0430\u043C\u0443\u0432\u0430\u043D\u043D\u044F\
  , \u0430\u043B\u0435\u2026"
lastmod: '2024-04-05T21:53:49.332666-06:00'
model: gpt-4-1106-preview
summary: "\u041F\u043E\u0448\u0443\u043A \u0456 \u0437\u0430\u043C\u0456\u043D\u0430\
  \ \u0442\u0435\u043A\u0441\u0442\u0443 \u0432 Elm \u0437\u0434\u0456\u0439\u0441\
  \u043D\u044E\u0454\u0442\u044C\u0441\u044F \u0447\u0435\u0440\u0435\u0437 \u043C\
  \u043E\u0434\u0443\u043B\u044C `Regex`."
title: "\u041F\u043E\u0448\u0443\u043A \u0442\u0430 \u0437\u0430\u043C\u0456\u043D\
  \u0430 \u0442\u0435\u043A\u0441\u0442\u0443"
weight: 10
---

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
