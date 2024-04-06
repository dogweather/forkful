---
date: 2024-01-20 17:33:10.429226-07:00
description: "How to: (\u042F\u043A \u0440\u043E\u0431\u0438\u0442\u0438:) \u0423\
  \ \u043C\u0438\u043D\u0443\u043B\u043E\u043C\u0443, \u0434\u0430\u0442\u0438 \u043F\
  \u043E\u0440\u0456\u0432\u043D\u044E\u0432\u0430\u043B\u0438, \u0432\u0438\u043A\
  \u043E\u0440\u0438\u0441\u0442\u043E\u0432\u0443\u044E\u0447\u0438 \u043A\u0430\u043B\
  \u0435\u043D\u0434\u0430\u0440\u0456 \u0456 \u043C\u0430\u0442\u0435\u043C\u0430\
  \u0442\u0438\u043A\u0443. \u0421\u044C\u043E\u0433\u043E\u0434\u043D\u0456, \u043C\
  \u0438 \u0432\u0438\u043A\u043E\u0440\u0438\u0441\u0442\u043E\u0432\u0443\u0454\u043C\
  \u043E \u043A\u043E\u043C\u043F'\u044E\u0442\u0435\u0440\u043D\u0456 \u0447\u0430\
  \u0441\u043E\u0432\u0456 \u043C\u0456\u0442\u043A\u0438 (Posix \u0443\u2026"
lastmod: '2024-04-05T22:51:02.272829-06:00'
model: gpt-4-1106-preview
summary: "(\u042F\u043A \u0440\u043E\u0431\u0438\u0442\u0438:) \u0423 \u043C\u0438\
  \u043D\u0443\u043B\u043E\u043C\u0443, \u0434\u0430\u0442\u0438 \u043F\u043E\u0440\
  \u0456\u0432\u043D\u044E\u0432\u0430\u043B\u0438, \u0432\u0438\u043A\u043E\u0440\
  \u0438\u0441\u0442\u043E\u0432\u0443\u044E\u0447\u0438 \u043A\u0430\u043B\u0435\u043D\
  \u0434\u0430\u0440\u0456 \u0456 \u043C\u0430\u0442\u0435\u043C\u0430\u0442\u0438\
  \u043A\u0443."
title: "\u041F\u043E\u0440\u0456\u0432\u043D\u044F\u043D\u043D\u044F \u0434\u0432\u043E\
  \u0445 \u0434\u0430\u0442"
weight: 27
---

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
