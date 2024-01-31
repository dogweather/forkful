---
title:                "Расчет даты в будущем или прошлом"
date:                  2024-01-28T23:55:58.670450-07:00
model:                 gpt-4-0125-preview
simple_title:         "Расчет даты в будущем или прошлом"

category:             "Haskell"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ru/haskell/calculating-a-date-in-the-future-or-past.md"
changelog:
  - 2024-01-28, gpt-4-0125-preview, translated from English
---

{{< edit_this_page >}}

## Что и почему?

Расчёт будущей или прошедшей даты означает нахождение даты до или после заданного количества дней, месяцев или лет от определённой отправной точки. Программисты делают это для таких вещей, как сроки действия, планирование или определение времени, прошедшего между событиями.

## Как это делается:

Haskell использует библиотеки, такие как `time`, для работы с датами. Вот как добавить дни или месяцы к дате или вычесть их, чтобы найти прошедшую дату.

```Haskell
import Data.Time

-- Добавить дни к текущей дате
addDaysToCurrent :: Integer -> IO Day
addDaysToCurrent n = do
  today <- getCurrentTime
  timezone <- getCurrentTimeZone
  let localToday = utcToLocalTime timezone today
  return $ addDays n (localDay localToday)

-- Использование: addDaysToCurrent 10 чтобы добавить 10 дней к текущей дате

-- Рассчитать будущую или прошедшую дату, добавив или вычтя дни
calculateDate :: Day -> Integer -> Day
calculateDate start n = addDays n start

-- Пример использования:
-- let futureDate = calculateDate (fromGregorian 2023 1 1) 90

-- Чтобы обработать месяцы и годы, мы используем `addGregorianMonthsClip` и `addGregorianYearsClip`
calculateDateMonths :: Day -> Integer -> Day
calculateDateMonths start n = addGregorianMonthsClip n start

-- Использование:
-- let futureMonth = calculateDateMonths (fromGregorian 2023 1 1) 2

-- Вывести дату в формате ГГГГ-ММ-ДД
printFormattedDate :: Day -> IO ()
printFormattedDate date = putStrLn $ formatTime defaultTimeLocale "%F" date

-- Использование:
-- printFormattedDate futureDate
```

## Подробнее

В Haskell мы часто обращаемся к библиотеке `time` для расчётов дат. Эта библиотека предоставляет типы и функции для арифметики DateTime, анализа и форматирования. Исторически люди вручную корректировали даты, но библиотеки, такие как `time`, обрабатывают особенности календарей (например, високосные годы).

Альтернативы `time` включают в себя `Data.Time.Calendar.OrdinalDate` и `Data.Time.Clock.POSIX` для различных нужд, например, работы с номерами недель или метками времени.

С точки зрения реализации, расчёт дат удивительно сложен. Даже с `time`, функции, такие как `addGregorianMonthsClip`, гарантируют, что полученная дата будет действительной. Например, добавление одного месяца к 31 января "обрежет" дату до последнего дня февраля (либо 28-го, либо 29-го), а не 3 марта.

## Смотрите также

- Библиотека Haskell `time`: http://hackage.haskell.org/package/time
- Руководство по дате и времени от Haskell School: https://school.haskellforall.com/#date-and-time
- Объяснение ZonedTime и UTC: https://www.47deg.com/blog/dealing-with-time-in-haskell/
