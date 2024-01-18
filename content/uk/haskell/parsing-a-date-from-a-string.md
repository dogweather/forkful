---
title:                "Розбір дати з рядка"
html_title:           "Haskell: Розбір дати з рядка"
simple_title:         "Розбір дати з рядка"
programming_language: "Haskell"
category:             "Haskell"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/uk/haskell/parsing-a-date-from-a-string.md"
---

{{< edit_this_page >}}

## Що & Чому?
Розбір дати з рядка - це процес перетворення дати, представленої у вигляді тексту, на тип даних, який можна обробляти у програмі. Програмісти зазвичай займаються цим, коли потрібно зчитати дату з файлу або з Інтернету та подальше використання її у своєму коді.

## Як робити:
```Haskell
import Data.Time.Format
import System.Locale
parseDate :: String -> Maybe Day
parseDate dateStr = parseTimeM True defaultTimeLocale "%Y-%m-%d" dateStr :: Maybe Day

-- Приклад вхідного рядка та вихідного результату:
-- parseDate "2020-01-01" --> Just 2020-01-01
-- parseDate "1/1/2020" --> Nothing (помилка)
```

## Глибока глибина:
Історичній контекст: розбір дат з рядка став можливим завдяки розвитку комп'ютерних технологій, які дозволили зберігати та обробляти дати як окремі значення, а не просто рядки. Існують різні альтернативи для розбору дат з рядка в Haskell, такі як використання бібліотеки Data.Time, яка містить багато корисних функцій для роботи з датами.

## Подивитися також:
- [Документація Data.Time.Format](https://hackage.haskell.org/package/time-1.9.3/docs/Data-Time-Format.html)
- [Стаття "Manipulating Time in Haskell"](https://blog.logrocket.com/manipulating-time-in-haskell/)