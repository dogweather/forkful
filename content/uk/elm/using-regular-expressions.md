---
title:                "Використання регулярних виразів"
html_title:           "Bash: Використання регулярних виразів"
simple_title:         "Використання регулярних виразів"
programming_language: "Elm"
category:             "Elm"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/uk/elm/using-regular-expressions.md"
---

{{< edit_this_page >}}

## Що це таке & Навіщо?
Регулярні вирази - це шаблони, що шукають певні текстові послідовності. Програмісти використовують їх для здійснення складного пошуку і маніпуляції даними - це швидко і універсально.

## Як це робити:
```Elm
import Regex exposing (..)

-- Знаходимо цифри в рядку
matchDigits : String -> List String
matchDigits str =
    case Regex.fromString "\\d+" of
        Nothing -> []
        Just re -> Regex.find (All re) str |> List.map .match

-- Приклад використання
sampleOutput : List String
sampleOutput =
    matchDigits "Elm 0.19.1 з'явився у 2018 році"

-- ['0', '19', '1', '2018']
```

## Підводне каміння
Історично, регулярні вирази беруть початок в теоретичній інформатиці і були популяризовані інструментами Unix. У Elm, вони реалізовані через модуль `Regex`, як у багатьох інших мовах. Існують альтернативи, наприклад, строга обробка рядків, але вони окремі кейси. Регулярні вирази в Elm виконуються через JavaScript, тому ефективність залежить від JS двигуна.

## Дивись також:
- Elm `Regex` documentation: [https://package.elm-lang.org/packages/elm/regex/latest](https://package.elm-lang.org/packages/elm/regex/latest)
- Introduction to Regular Expressions: [https://developer.mozilla.org/en-US/docs/Web/JavaScript/Guide/Regular_Expressions](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Guide/Regular_Expressions)
- Online Regex Tester: [https://regex101.com/](https://regex101.com/)
