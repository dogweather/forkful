---
title:                "Розбір дати з рядка"
aliases:
- /uk/haskell/parsing-a-date-from-a-string.md
date:                  2024-02-03T19:14:36.448196-07:00
model:                 gpt-4-0125-preview
simple_title:         "Розбір дати з рядка"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/uk/haskell/parsing-a-date-from-a-string.md"
changelog:
  - 2024-02-03, gpt-4-0125-preview, translated from English
---

{{< edit_this_page >}}

## Що і чому?

Розбір дати з рядка в Haskell полягає в перетворенні текстових представлень дат у структурований формат, який програма може маніпулювати. Цей процес є фундаментальним для додатків, що працюють з календарними даними, дозволяючи виконувати функції, такі як обчислення тривалості, планування та валідація даних.

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
