---
date: 2024-01-20 17:45:44.695467-07:00
description: "\u042F\u043A \u0446\u0435 \u0440\u043E\u0431\u0438\u0442\u0438: ."
lastmod: '2024-03-13T22:44:49.132717-06:00'
model: gpt-4-1106-preview
summary: .
title: "\u0412\u0438\u0434\u0456\u043B\u0435\u043D\u043D\u044F \u043F\u0456\u0434\u0440\
  \u044F\u0434\u043A\u0456\u0432"
weight: 6
---

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
