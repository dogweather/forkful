---
title:                "Порівняння двох дат"
html_title:           "Elm: Порівняння двох дат"
simple_title:         "Порівняння двох дат"
programming_language: "Elm"
category:             "Elm"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/uk/elm/comparing-two-dates.md"
---

{{< edit_this_page >}}

## Чому

Порівняння двох дат є важливою задачею в програмуванні, оскільки деякі додатки можуть потребувати знаходження різниці між двома датами або визначення, яка дата є раніше або пізніше.

## Як

Для порівняння дат використовується функція `compare` вбудованого модулю `Date`. Приймаючи два значення типу `Date`, вона повертає `LT` (менше), `GT` (більше) або `EQ` (рівне), залежно від того, яка з дат є раніше чи пізніше.

```Elm
import Date exposing (compare)

compare (Date.fromIsoString "2020-10-01") (Date.fromIsoString "2020-12-05") -- поверне GT
compare (Date.fromIsoString "2020-02-15") (Date.fromIsoString "2020-01-01") -- поверне LT
compare (Date.fromIsoString "2020-03-07") (Date.fromIsoString "2020-03-07") -- поверне EQ
```

## Глибоке занурення

Крім функції `compare`, модуль `Date` також містить функції для створення і перетворення дат, а також для взаємодії з часовим зонами. Для отримання докладнішої інформації про можливі операції з датами в Elm, ознайомтеся з документацією на офіційному сайті.

## Дивіться також

- [Офіційна документація Elm про модуль `Date`](https://package.elm-lang.org/packages/elm/time/latest/Date)
- [Порівняння екземплярів `Date` на Stack Overflow](https://stackoverflow.com/questions/48348450/elm-compare-two-dates-and-return-value)