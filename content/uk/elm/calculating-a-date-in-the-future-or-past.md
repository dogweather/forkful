---
title:    "Elm: Розрахунок дати в майбутньому або минулому"
keywords: ["Elm"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/uk/elm/calculating-a-date-in-the-future-or-past.md"
---

{{< edit_this_page >}}

## Чому 

Розрахунок дати в майбутньому чи минулому може бути корисним для створення різноманітних додатків, таких як календарі, напоминалки та додатки для планування подій. Також це може бути корисно для створення функцій для обробки даних в бізнес-додатках. 

## Як

```Elm
import Date exposing (Date)
import Time exposing (Posix)
import Time.Extra exposing (fromCalendarDate)

-- Створення функції для розрахунку дати в майбутньому
futureDate : Int -> Posix -> Date
futureDate days current =
    current
        |> Time.add (Time.days days)
        |> fromCalendarDate

-- Створення функції для розрахунку дати в минулому
pastDate : Int -> Posix -> Date
pastDate days current =
    current
        |> Time.sub (Time.days days)
        |> fromCalendarDate

main =
    let
        today = Time.now |> fromCalendarDate
        future = futureDate 30 today
        past = pastDate 14 today
    in
    [ "Сьогодні: " ++ toString today
    , "Дата в майбутньому: " ++ toString future
    , "Дата в минулому: " ++ toString past
    ]

-- Вихід:
-- [
--   "Сьогодні: Date 2020 6 25"
--   "Дата в майбутньому: Date 2020 7 25"
--   "Дата в минулому: Date 2020 6 11"
-- ]

``` 

## Глибше 

За допомогою бібліотеки `Date` та функцій `Time` та `Time.Extra` ми можемо змістити поточну дату вперед або назад на довільну кількість днів. При цьому, якщо поточна дата не враховує фактори, які можуть вплинути на перехід на новий місяць або рік, функції `fromCalendarDate` автоматично врахують ці фактори і повернуть правильну дату. 

## Дивись також 

- [Docs for Date module](https://package.elm-lang.org/packages/elm/time/latest/Date)
- [Docs for Time module](https://package.elm-lang.org/packages/elm/time/latest/Time)
- [Docs for Time.Extra module](https://package.elm-lang.org/packages/elm/time/latest/Time-Extra)