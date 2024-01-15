---
title:                "Порівняння двох дат"
html_title:           "Haskell: Порівняння двох дат"
simple_title:         "Порівняння двох дат"
programming_language: "Haskell"
category:             "Haskell"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/uk/haskell/comparing-two-dates.md"
---

{{< edit_this_page >}}

## Зачем

Існує багато ситуацій, коли нам потрібно порівняти дві дати. Наприклад, під час роботи з даними, при розробці програмного забезпечення або просто для власних потреб. Особливо це актуально в країнах, де використовуються різні формати запису дат.

## Як це зробити

Для порівняння двох дат використовується функція `compare`. Вона приймає два аргументи типу `Day` (дата) та повертає значення типу `Ordering`, яке може бути `LT` (менше), `EQ` (рівне) або `GT` (більше). Давайте розглянемо приклад, де ми порівнюємо дві дати:

```Haskell
import Data.Time.Calendar

date1 = fromGregorian 2021 10 1
date2 = fromGregorian 2021 11 1

compare date1 date2
```
Результатом буде `LT`, оскільки `date1` є меншою за `date2` (10 жовтня проти 1 листопада).

Також, є можливість використовувати інші функції для більш складного порівняння, наприклад `compare` з використанням `ZonedTime`, `UTCTime` або `LocalTime`.

## Поглиблене вивчення

Якщо вам потрібно виконати більш точне порівняння двох дат, ви можете використовувати функцію `diffDays`. Вона приймає два аргументи типу `Day` та повертає різницю в днях між ними. В цьому випадку, якщо обидві дати будуть однакові, результат буде 0.

```Haskell
date1 = fromGregorian 2021 10 1
date2 = fromGregorian 2021 11 1

diffDays date1 date2
```
Результатом буде `-31`, оскільки `date1` є 31 днем перед `date2`.

Іншою корисною функцією є `isSameDay`, яка приймає два аргументи типу `Day` та повертає `True` або `False`, в залежності від того, чи є дати однаковими.

## Дивіться також

- [Модуль Data.Time.Calendar](https://hackage.haskell.org/package/time/docs/Data-Time-Calendar.html)
- [Функція compare](https://hackage.haskell.org/package/time/docs/Data-Time-Calendar.html#v:compare)
- [Функція diffDays](https://hackage.haskell.org/package/time/docs/Data-Time-Calendar.html#v:diffDays)
- [Функція isSameDay](https://hackage.haskell.org/package/time/docs/Data-Time-Calendar.html#v:isSameDay)