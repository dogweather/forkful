---
title:                "Получение текущей даты"
aliases: - /ru/haskell/getting-the-current-date.md
date:                  2024-01-28T23:58:34.247751-07:00
model:                 gpt-4-0125-preview
simple_title:         "Получение текущей даты"

tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ru/haskell/getting-the-current-date.md"
changelog:
  - 2024-01-28, gpt-4-0125-preview, translated from English
---

{{< edit_this_page >}}

## Что и Почему?
Получение текущей даты в вашем коде позволяет маркировать события в момент их происходения. Это ключевой момент для ведения журналов, отслеживания временно-чувствительных данных и настройки пользовательского опыта на основе даты.

## Как:
В Haskell получить текущую дату можно используя библиотеку `Data.Time`. Сначала импортируйте необходимое:

```haskell
import Data.Time
```

Теперь захватите сегодняшнюю дату:

```haskell
main :: IO ()
main = do
    today <- getCurrentTime
    putStrLn $ "Сегодняшняя дата: " ++ show (utctDay today)
```

Пример вывода может выглядеть следующим образом:

```
Сегодняшняя дата: 2023-03-23
```

## Глубокое Погружение
Haskell работает с датой-временем начиная с его ранних дней, библиотека `Data.Time` развивалась из более старых библиотек времени. У неё есть всё необходимое "из коробки", но она может показаться немного пугающей. Существуют альтернативы, например, `time-recurrence` для расчётов даты по шаблонам, или `old-time`, прежняя основная библиотека Haskell для операций с датой-временем.

`Data.Time` много работает с `UTCTime`, универсальным стандартом времени. Но вы также можете работать с временными зонами используя `ZonedTime` из той же библиотеки. Она работает, комбинируя `LocalTime` (дата и время без зоны) и `TimeZone`, который указывает смещение от `UTC`.

## Смотрите также
- "Learn You a Haskell" для операций со временем: [http://learnyouahaskell.com](http://learnyouahaskell.com/)
- Управление временными зонами в Haskell: [https://hackage.haskell.org/package/time-1.9.3/docs/Data-Time-LocalTime.html](https://hackage.haskell.org/package/time-1.9.3/docs/Data-Time-LocalTime.html)
