---
title:                "Розбір дати зі строки"
html_title:           "Elm: Розбір дати зі строки"
simple_title:         "Розбір дати зі строки"
programming_language: "Elm"
category:             "Elm"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/uk/elm/parsing-a-date-from-a-string.md"
---

{{< edit_this_page >}}

## What & Why?
Парсинг дати з рядка - це процес перетворення дати, що зберігається у вигляді рядка, до спеціального об'єкту дати, який можна використовувати для подальшої обробки та маніпуляцій. Програмісти часто використовують парсинг дати з рядка, оскільки це дозволяє їм змінювати та аналізувати дані з зручними інструментами.

## How to:
```elm
import Date exposing (Date, fromIsoString)

dateString = "2021-03-18"
date = fromIsoString dateString
-- date = Ok (Date 2021 Month.mar 18)

invalidDateString = "2021-13-01"
date = fromIsoString invalidDateString
-- date = Err "invalid month"
```

## Deep Dive:
Парсинг дати з рядка є важливим елементом програмування, оскільки дати часто зберігаються у вигляді рядків у базах даних чи отримуються з вхідних даних. Існує кілька альтернативних методів парсингу дати з рядка, наприклад, використання регулярних виразів або сторонніх бібліотек. У реалізації Elm, метод ```fromIsoString```, що використовує стандартний формат ISO 8601, є надійною та зручною опцією для парсингу дати з рядка.

## See Also:
- [Official Elm Date Documentation](https://package.elm-lang.org/packages/elm/core/latest/Date)
- [ISO 8601 Date Format](https://www.iso.org/iso-8601-date-and-time-format.html)
- [Regular Expressions in Elm by Richard Feldman](https://www.youtube.com/watch?v=4LOIY9i_BeE)