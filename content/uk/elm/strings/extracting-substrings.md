---
title:                "Виділення підрядків"
aliases: - /uk/elm/extracting-substrings.md
date:                  2024-01-20T17:45:44.695467-07:00
model:                 gpt-4-1106-preview
simple_title:         "Виділення підрядків"

tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/uk/elm/extracting-substrings.md"
---

{{< edit_this_page >}}

## Що і Навіщо?
Витягування підрядків – це отримання кусочків тексту з більшого рядка. Програмісти використовують це для аналізу тексту, валідації, чи просто щоб показати важливу інформацію користувачам.

## Як це робити:
```Elm
import String exposing (slice)

extractSub : String -> Int -> Int -> String
extractSub str start end =
    slice start end str

main =
    let
        fullText = "Вітаємо в Ельм!"
        subText = extractSub fullText 0 7
    in
    -- Виведення: "Вітаємо"
    Text.fromString subText
```

## Поглиблений Розгляд
Витягування підрядків в Elm здійснюється через функцію `slice`, яка є частиною стандартної бібліотеки `String`. У минулому, рядкові операції часто здійснювалися іншими функціями або бібліотеками, але `slice` стала зручним вбудованим рішенням.

Функція `slice` приймає три аргументи: початковий індекс, кінцевий індекс, і сам рядок. Індекси в Elm починаються з 0. Кінцевий індекс не включає символ у підрядку, тому слід додати 1, щоб включити останній символ.

Окрім `slice`, можна використовувати інші функції, такі як `left`, `right`, та `drop`, які дають різні способи витягнути підрядок залежно від вашої потреби.

## Дивіться Також
1. Elm String documentation: [https://package.elm-lang.org/packages/elm/core/latest/String](https://package.elm-lang.org/packages/elm/core/latest/String)
