---
title:                "Порівняння двох дат"
date:                  2024-01-20T17:33:10.429226-07:00
model:                 gpt-4-1106-preview
simple_title:         "Порівняння двох дат"
programming_language: "Elm"
category:             "Elm"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/uk/elm/comparing-two-dates.md"
---

{{< edit_this_page >}}

## What & Why? (Що і чому?)
Сравнение двух дат - це перевірка яка з дат раніша або пізніша. Програмісти роблять це для управління термінами, подіями і планування.

## How to: (Як робити:)
```Elm
import Time exposing (Posix)
import Date

compareDates : Posix -> Posix -> Order
compareDates date1 date2 =
    if date1 < date2 then
        LT
    else if date1 > date2 then
        GT
    else
        EQ

-- Приклад використання:
date1 : Posix
date1 =
    Date.fromIsoString "2023-03-01T00:00:00Z" |> Result.withDefault (Date.fromTime 0)

date2 : Posix
date2 =
    Date.fromIsoString "2023-04-01T00:00:00Z" |> Result.withDefault (Date.fromTime 0)

result : Order
result = compareDates date1 date2
-- result буде LT, оскільки date1 раніше ніж date2
```

## Deep Dive (Поглиблений аналіз)
У минулому, дати порівнювали, використовуючи календарі і математику. Сьогодні, ми використовуємо комп'ютерні часові мітки (Posix у Elm), які дозволяють точно порівняти дати. Є альтернативи Elm, наприклад JavaScript's `Date`, але Elm пропонує імутабельність і надійність. Деталі реалізації: `Posix` - це кількість мілісекунд з півночі 1 січня 1970 UTC, в Elm його зазвичай отримують з `Date`.

## See Also (Дивіться також)
- [Elm Time module](https://package.elm-lang.org/packages/elm/time/latest/)
- [Elm Date example apps](https://elm-lang.org/examples/time)
- [Elm Date library](https://package.elm-lang.org/packages/justinmimbs/date/latest/)