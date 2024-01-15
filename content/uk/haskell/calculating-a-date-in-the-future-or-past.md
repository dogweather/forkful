---
title:                "Обчислення дати в майбутньому або минулому"
html_title:           "Haskell: Обчислення дати в майбутньому або минулому"
simple_title:         "Обчислення дати в майбутньому або минулому"
programming_language: "Haskell"
category:             "Haskell"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/uk/haskell/calculating-a-date-in-the-future-or-past.md"
---

{{< edit_this_page >}}

# Чому

Існує безліч ситуацій, коли потрібно визначити дату у майбутньому або минулому. Наприклад, ви можете хотіти знати, коли буде ваш наступний день народження або коли закінчиться термін дії договору. У таких випадках корисним буде мати засоби для розрахунку дат, що лежать у майбутньому або минулому.

# Як це зробити

За допомогою функцій з модуля `Data.Time`, ми можемо легко визначити дату у майбутньому або минулому. Припустимо, нам потрібно знайти дату через 10 днів від поточного часу:

```Haskell
import Data.Time

main = do
  currentDate <- getCurrentTime
  let futureDate = addDays 10 $ utctDay currentDate
  print futureDate
```
Виведе: `2021-06-28`

Або, якщо нам потрібно знайти дату, що лежить за 2 роки від поточного часу:

```Haskell
import Data.Time

main = do
  currentDate <- getCurrentTime
  let futureDate = addGregorianYearsClip 2 $ utctDay currentDate
  print futureDate
```
Виведе: `2023-05-18`

# Глибоке погруження

Окрім додавання днів та років, функції модуля `Data.Time` дозволяють виконувати більш складні операції з датами. Наприклад, ви можете отримати день тижня для певної дати за допомогою функції `dayOfWeek`:

```Haskell
import Data.Time
import Data.Time.Calendar.WeekDate

main = do
  currentDate <- getCurrentTime
  let (_, _, dayOfWeek) = toWeekDate $ utctDay currentDate
  print dayOfWeek
```
Виведе: `3` (де 1 - понеділок, 7 - неділя)

Або, ви можете порівнювати дати за допомогою функції `diffDays`, яка повертає різницю між датами у днях:

```Haskell
import Data.Time

main = do
  currentDate <- getCurrentTime
  let birthDate = fromGregorian 1995 10 5
  let daysPassed = diffDays (utctDay currentDate) birthDate
  print daysPassed
```
Виведе: `9331` (скільки днів минуло з дня народження)

# Дивись також

- [Hackage - Data.Time](https://hackage.haskell.org/package/time/docs/Data-Time.html)
- [Learn You a Haskell - Date and Time](http://learnyouahaskell.com/datetime)
- [Real World Haskell - Chapter 7: I/O for people in a hurry](http://book.realworldhaskell.org/read/io.html)