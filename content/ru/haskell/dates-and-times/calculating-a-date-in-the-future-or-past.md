---
changelog:
- 2024-01-28, gpt-4-0125-preview, translated from English
date: 2024-01-28 23:55:58.670450-07:00
description: "\u0420\u0430\u0441\u0447\u0451\u0442 \u0431\u0443\u0434\u0443\u0449\u0435\
  \u0439 \u0438\u043B\u0438 \u043F\u0440\u043E\u0448\u0435\u0434\u0448\u0435\u0439\
  \ \u0434\u0430\u0442\u044B \u043E\u0437\u043D\u0430\u0447\u0430\u0435\u0442 \u043D\
  \u0430\u0445\u043E\u0436\u0434\u0435\u043D\u0438\u0435 \u0434\u0430\u0442\u044B\
  \ \u0434\u043E \u0438\u043B\u0438 \u043F\u043E\u0441\u043B\u0435 \u0437\u0430\u0434\
  \u0430\u043D\u043D\u043E\u0433\u043E \u043A\u043E\u043B\u0438\u0447\u0435\u0441\u0442\
  \u0432\u0430 \u0434\u043D\u0435\u0439, \u043C\u0435\u0441\u044F\u0446\u0435\u0432\
  \ \u0438\u043B\u0438 \u043B\u0435\u0442 \u043E\u0442 \u043E\u043F\u0440\u0435\u0434\
  \u0435\u043B\u0451\u043D\u043D\u043E\u0439 \u043E\u0442\u043F\u0440\u0430\u0432\u043D\
  \u043E\u0439 \u0442\u043E\u0447\u043A\u0438.\u2026"
lastmod: '2024-03-13T22:44:45.158187-06:00'
model: gpt-4-0125-preview
summary: "\u0420\u0430\u0441\u0447\u0451\u0442 \u0431\u0443\u0434\u0443\u0449\u0435\
  \u0439 \u0438\u043B\u0438 \u043F\u0440\u043E\u0448\u0435\u0434\u0448\u0435\u0439\
  \ \u0434\u0430\u0442\u044B \u043E\u0437\u043D\u0430\u0447\u0430\u0435\u0442 \u043D\
  \u0430\u0445\u043E\u0436\u0434\u0435\u043D\u0438\u0435 \u0434\u0430\u0442\u044B\
  \ \u0434\u043E \u0438\u043B\u0438 \u043F\u043E\u0441\u043B\u0435 \u0437\u0430\u0434\
  \u0430\u043D\u043D\u043E\u0433\u043E \u043A\u043E\u043B\u0438\u0447\u0435\u0441\u0442\
  \u0432\u0430 \u0434\u043D\u0435\u0439, \u043C\u0435\u0441\u044F\u0446\u0435\u0432\
  \ \u0438\u043B\u0438 \u043B\u0435\u0442 \u043E\u0442 \u043E\u043F\u0440\u0435\u0434\
  \u0435\u043B\u0451\u043D\u043D\u043E\u0439 \u043E\u0442\u043F\u0440\u0430\u0432\u043D\
  \u043E\u0439 \u0442\u043E\u0447\u043A\u0438."
title: "\u0420\u0430\u0441\u0447\u0435\u0442 \u0434\u0430\u0442\u044B \u0432 \u0431\
  \u0443\u0434\u0443\u0449\u0435\u043C \u0438\u043B\u0438 \u043F\u0440\u043E\u0448\
  \u043B\u043E\u043C"
weight: 26
---

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
