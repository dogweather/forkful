---
date: 2024-01-20 17:33:10.429226-07:00
description: "\u0421\u0440\u0430\u0432\u043D\u0435\u043D\u0438\u0435 \u0434\u0432\u0443\
  \u0445 \u0434\u0430\u0442 - \u0446\u0435 \u043F\u0435\u0440\u0435\u0432\u0456\u0440\
  \u043A\u0430 \u044F\u043A\u0430 \u0437 \u0434\u0430\u0442 \u0440\u0430\u043D\u0456\
  \u0448\u0430 \u0430\u0431\u043E \u043F\u0456\u0437\u043D\u0456\u0448\u0430. \u041F\
  \u0440\u043E\u0433\u0440\u0430\u043C\u0456\u0441\u0442\u0438 \u0440\u043E\u0431\u043B\
  \u044F\u0442\u044C \u0446\u0435 \u0434\u043B\u044F \u0443\u043F\u0440\u0430\u0432\
  \u043B\u0456\u043D\u043D\u044F \u0442\u0435\u0440\u043C\u0456\u043D\u0430\u043C\u0438\
  , \u043F\u043E\u0434\u0456\u044F\u043C\u0438 \u0456 \u043F\u043B\u0430\u043D\u0443\
  \u0432\u0430\u043D\u043D\u044F."
lastmod: '2024-03-13T22:44:49.171089-06:00'
model: gpt-4-1106-preview
summary: "\u0421\u0440\u0430\u0432\u043D\u0435\u043D\u0438\u0435 \u0434\u0432\u0443\
  \u0445 \u0434\u0430\u0442 - \u0446\u0435 \u043F\u0435\u0440\u0435\u0432\u0456\u0440\
  \u043A\u0430 \u044F\u043A\u0430 \u0437 \u0434\u0430\u0442 \u0440\u0430\u043D\u0456\
  \u0448\u0430 \u0430\u0431\u043E \u043F\u0456\u0437\u043D\u0456\u0448\u0430. \u041F\
  \u0440\u043E\u0433\u0440\u0430\u043C\u0456\u0441\u0442\u0438 \u0440\u043E\u0431\u043B\
  \u044F\u0442\u044C \u0446\u0435 \u0434\u043B\u044F \u0443\u043F\u0440\u0430\u0432\
  \u043B\u0456\u043D\u043D\u044F \u0442\u0435\u0440\u043C\u0456\u043D\u0430\u043C\u0438\
  , \u043F\u043E\u0434\u0456\u044F\u043C\u0438 \u0456 \u043F\u043B\u0430\u043D\u0443\
  \u0432\u0430\u043D\u043D\u044F."
title: "\u041F\u043E\u0440\u0456\u0432\u043D\u044F\u043D\u043D\u044F \u0434\u0432\u043E\
  \u0445 \u0434\u0430\u0442"
weight: 27
---

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
