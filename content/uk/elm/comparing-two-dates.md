---
title:                "Порівняння двох дат"
html_title:           "Clojure: Порівняння двох дат"
simple_title:         "Порівняння двох дат"
programming_language: "Elm"
category:             "Elm"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/uk/elm/comparing-two-dates.md"
---

{{< edit_this_page >}}

## Що і чому?
Порівняння двох дат - це процес визначення, яка дата раніша або пізніша. Програмісти роблять це для управління часовими рамками у додатках, наприклад, для виведення терміну дії абонементу або встановлення дати закінчення задачі.

## Як це зробити:
Ось приклад коду, який порівнює дві дати в Elm:
```Elm
module Main exposing (..)

import Time exposing (..)

main =
    let
        date1 = Date.fromTime 1632502302000
        date2 = Date.fromTime 1532502302000
    in
    if Date.compare date1 date2 == GT then
        "Date1 is later"
    else if Date.compare date1 date2 == EQ then
        "Both dates are same"
    else
        "Date2 is later"
```
Виходом цього коду буде: `"Date1 is later"`, тому що `1632502302000` пізніше за `1532502302000`.

## Занурення глибше:
1) **Історичний контекст**: Elm - це сучасна мова програмування, яка є правильнішим, безпечнішим та зручнішим варіантом для багатьох клієнтських веб-програм.

2) **Альтернативи**: Інші мови програмування, такі як JavaScript, Python та Java, також мають можливість порівняння дат, але підхід Elm більш строгий та консистентний.

3) **Реалізація**: Elm використовує типи даних `Date` та `Posix` для роботи з датами та часом. Функція `Date.compare` повертає значення типу `Order`, яке може бути `LT` (менше), `EQ` (рівне) або `GT` (більше).

## Дивіться також:
- [Офіційна документація Elm про дати та час](https://package.elm-lang.org/packages/elm/time/latest/)
- [Гарний вступ до Elm](https://elmprogramming.com/)
- [Форуми Elm для допомоги](https://discourse.elm-lang.org/)