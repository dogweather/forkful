---
changelog:
- 2024-01-28, gpt-4-0125-preview, translated from English
date: 2024-01-28 23:55:52.963944-07:00
description: "\u041A\u0430\u043A \u044D\u0442\u043E \u0441\u0434\u0435\u043B\u0430\
  \u0442\u044C: Elm \u0443\u043F\u0440\u043E\u0449\u0430\u0435\u0442 \u0441\u0440\u0430\
  \u0432\u043D\u0435\u043D\u0438\u0435 \u0434\u0430\u0442. \u0414\u043E\u043F\u0443\
  \u0441\u0442\u0438\u043C, \u0443 \u0432\u0430\u0441 \u0435\u0441\u0442\u044C \u0434\
  \u0432\u0435 \u0434\u0430\u0442\u044B. \u0412\u043E\u0442 \u043A\u0430\u043A \u0432\
  \u044B \u043C\u043E\u0433\u043B\u0438 \u0431\u044B \u043F\u0440\u043E\u0432\u0435\
  \u0440\u0438\u0442\u044C, \u043A\u0430\u043A\u0430\u044F \u0438\u0437 \u043D\u0438\
  \u0445 \u0438\u0434\u0435\u0442 \u043F\u0435\u0440\u0432\u043E\u0439."
lastmod: '2024-03-13T22:44:44.924514-06:00'
model: gpt-4-0125-preview
summary: "Elm \u0443\u043F\u0440\u043E\u0449\u0430\u0435\u0442 \u0441\u0440\u0430\u0432\
  \u043D\u0435\u043D\u0438\u0435 \u0434\u0430\u0442."
title: "\u0421\u0440\u0430\u0432\u043D\u0435\u043D\u0438\u0435 \u0434\u0432\u0443\u0445\
  \ \u0434\u0430\u0442"
weight: 27
---

## Как это сделать:
Elm упрощает сравнение дат. Допустим, у вас есть две даты. Вот как вы могли бы проверить, какая из них идет первой:

```Elm
import Time exposing (Posix)
import Date

compareDates : Posix -> Posix -> Order
compareDates date1 date2 =
    if date1 < date2 then
        LT  -- date1 раньше чем date2
    else if date1 > date2 then
        GT  -- date1 позже чем date2
    else
        EQ  -- даты одинаковы

-- Пример использования:
let
    date1 = Date.fromPosix <| Time.millisToPosix 1650931200000 -- Добавьте вашу первую дату в POSIX время
    date2 = Date.fromPosix <| Time.millisToPosix 1651017600000 -- И вашу вторую дату в POSIX время
in
compareDates date1 date2
-- Вывод будет либо LT, GT или EQ
```

Вы также можете рассчитать разницу в миллисекундах:

```Elm
timeDifference : Posix -> Posix -> Time.Duration
timeDifference date1 date2 =
    Time.millisToPosix date1 - Time.millisToPosix date2

-- Пример использования:
let
    date1 = Date.fromPosix <| Time.millisToPosix 1650931200000
    date2 = Date.fromPosix <| Time.millisToPosix 1651017600000
in
timeDifference date1 date2
-- Вывод: Продолжительность в миллисекундах
```

## Подробнее
Elm хранит даты в формате `Posix`, представляющем миллисекунды с эпохи Unix (1 января 1970 года, UTC). Это распространенный подход, который разделяет свои корни с Unix Time, и он облегчает манипуляцию и хранение дат.

Хотя базовая библиотека Elm предоставляет основные возможности для работы с датами, существуют альтернативы, такие как `justinmimbs/date`, для более сложных операций.

При реализации сравнения дат помните, что часовые пояса могут усложнить задачу. Модуль `Time` в Elm предполагает использование UTC, что избавляет вас от проблем с переходом на летнее время, но вам может потребоваться корректировка для локальных часовых поясов в вашем приложении.

## См. также
- Модуль времени Elm: https://package.elm-lang.org/packages/elm/time/latest/
- Пакет Даты от Джастина Мимбса для Elm: https://package.elm-lang.org/packages/justinmimbs/date/latest/
- Время Unix: https://en.wikipedia.org/wiki/Unix_time
