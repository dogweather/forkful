---
changelog:
- 2024-02-03, gpt-4-0125-preview, translated from English
date: 2024-02-03 19:14:36.448196-07:00
description: "\u042F\u043A: \"\u0417 \u043A\u043E\u0440\u043E\u0431\u043A\u0438\"\
  \ Haskell \u043F\u0440\u043E\u043F\u043E\u043D\u0443\u0454 \u0431\u0430\u0437\u043E\
  \u0432\u0456 \u0456\u043D\u0441\u0442\u0440\u0443\u043C\u0435\u043D\u0442\u0438\
  \ \u0434\u043B\u044F \u0440\u043E\u0437\u0431\u043E\u0440\u0443 \u0434\u0430\u0442\
  , \u0430\u043B\u0435 \u0432\u0438\u043A\u043E\u0440\u0438\u0441\u0442\u0430\u043D\
  \u043D\u044F \u0431\u0456\u0431\u043B\u0456\u043E\u0442\u0435\u043A, \u0442\u0430\
  \u043A\u0438\u0445 \u044F\u043A `time` \u0434\u043B\u044F \u043E\u0441\u043D\u043E\
  \u0432\u043D\u043E\u0457 \u0444\u0443\u043D\u043A\u0446\u0456\u043E\u043D\u0430\u043B\
  \u044C\u043D\u043E\u0441\u0442\u0456, \u0442\u0430 `date-\u2026"
lastmod: '2024-03-13T22:44:49.375934-06:00'
model: gpt-4-0125-preview
summary: "\"\u0417 \u043A\u043E\u0440\u043E\u0431\u043A\u0438\" Haskell \u043F\u0440\
  \u043E\u043F\u043E\u043D\u0443\u0454 \u0431\u0430\u0437\u043E\u0432\u0456 \u0456\
  \u043D\u0441\u0442\u0440\u0443\u043C\u0435\u043D\u0442\u0438 \u0434\u043B\u044F\
  \ \u0440\u043E\u0437\u0431\u043E\u0440\u0443 \u0434\u0430\u0442, \u0430\u043B\u0435\
  \ \u0432\u0438\u043A\u043E\u0440\u0438\u0441\u0442\u0430\u043D\u043D\u044F \u0431\
  \u0456\u0431\u043B\u0456\u043E\u0442\u0435\u043A, \u0442\u0430\u043A\u0438\u0445\
  \ \u044F\u043A `time` \u0434\u043B\u044F \u043E\u0441\u043D\u043E\u0432\u043D\u043E\
  \u0457 \u0444\u0443\u043D\u043A\u0446\u0456\u043E\u043D\u0430\u043B\u044C\u043D\u043E\
  \u0441\u0442\u0456, \u0442\u0430 `date-parse` \u0430\u0431\u043E `time-parse` \u0434\
  \u043B\u044F \u0431\u0456\u043B\u044C\u0448 \u0433\u043D\u0443\u0447\u043A\u043E\
  \u0433\u043E \u0440\u043E\u0437\u0431\u043E\u0440\u0443, \u043C\u043E\u0436\u0435\
  \ \u0437\u043D\u0430\u0447\u043D\u043E \u0441\u043F\u0440\u043E\u0441\u0442\u0438\
  \u0442\u0438 \u0437\u0430\u0432\u0434\u0430\u043D\u043D\u044F."
title: "\u0420\u043E\u0437\u0431\u0456\u0440 \u0434\u0430\u0442\u0438 \u0437 \u0440\
  \u044F\u0434\u043A\u0430"
weight: 30
---

## Як:
"З коробки" Haskell пропонує базові інструменти для розбору дат, але використання бібліотек, таких як `time` для основної функціональності, та `date-parse` або `time-parse` для більш гнучкого розбору, може значно спростити завдання.

Спочатку переконайтеся, що у вас є бібліотека `time`; часто вона включена у GHC, але якщо вам потрібно визначити її як залежність, додайте `time` до файлу cabal вашого проєкту або використовуйте `cabal install time` для її ручної установки.

```haskell
import Data.Time.Format
import Data.Time.Clock
import System.Locale (defaultTimeLocale)

-- Використання бібліотеки time для розбору дати у стандартному форматі
parseBasicDate :: String -> Maybe UTCTime
parseBasicDate = parseTimeM True defaultTimeLocale "%Y-%m-%d" 
```

Приклад використання та виводу:

```haskell
main :: IO ()
main = print $ parseBasicDate "2023-04-01"

-- Вивід: Just 2023-03-31 22:00:00 UTC
```

Для більш складних сценаріїв, коли вам потрібно обробляти кілька форматів або локалей, бібліотеки сторонніх виробників, такі як `date-parse`, можуть бути зручнішими:

Припускаючи, що ви додали `date-parse` до своїх залежностей і встановили його, ось як ви могли б його використовувати:

```haskell
import Data.Time.Calendar
import Text.Date.Parse (parseDate)

-- Розбір рядка дати з бібліотекою date-parse підтримує кілька форматів
parseFlexibleDate :: String -> Maybe Day
parseFlexibleDate = parseDate
```

Приклад використання з `date-parse`:

```haskell
main :: IO ()
main = print $ parseFlexibleDate "April 1, 2023"

-- Вивід: Just 2023-04-01
```

Кожен приклад демонструє основний підхід до перетворення рядка в корисний об'єкт дати в Haskell. Вибір між використанням вбудованих функцій бібліотеки `time` та вибором рішення сторонніх виробників, таких як `date-parse`, залежить від конкретних потреб вашого додатку, таких як діапазон форматів вводу, які вам потрібно обробити.
