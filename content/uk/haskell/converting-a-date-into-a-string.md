---
title:                "Перетворення дати у рядок"
html_title:           "Haskell: Перетворення дати у рядок"
simple_title:         "Перетворення дати у рядок"
programming_language: "Haskell"
category:             "Haskell"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/uk/haskell/converting-a-date-into-a-string.md"
---

{{< edit_this_page >}}

## Чому

Велика частина роботи програміста полягає у роботі з датами. Конвертування дати в рядок може бути корисним для звітування, збереження дати у базі даних або для відображення у користувацькому інтерфейсі. Ця стаття допоможе вам зрозуміти, як перетворити дату в рядок, використовуючи мову програмування Haskell.

## Як

У Haskell існує багато способів конвертування дати в рядок. Найпростішим способом є використання функції `show` для перетворення дати у рядок. Припустимо, ми маємо дату 15 червня 2021 року. Нижче наведений приклад коду та його виведення:

```Haskell
show (2021, 06, 15)
-- Приклад виведення: "(2021, 06, 15)"
```

Іншим способом є використання модуля `Data.Time.Format` для визначення бажаного формату рядка. Наприклад, якщо ми хочемо отримати дату у форматі `MM/DD/YYYY` (місяць/день/рік), ми можемо використати функцію `formatTime` з наступними параметрами: формат рядка, дата, і вихідний рядок для форматування. Ось приклад коду та його виведення:

```Haskell
import Data.Time.Format
import Data.Time.Calendar
import Data.Time.Clock

let currentDate = utctDay getCurrentTime
formatTime defaultTimeLocale "%m/%d/%Y" currentDate
-- Приклад виведення: "06/15/2021"
```

## Поглиблене дослідження

Якщо ви хочете детальніше ознайомитися з роботою з датами у Haskell, вам може бути цікаво дослідити інші методи конвертації дати в рядок, такі як використання пакету `time`, або звернення до стандартів ISO 8601 та RFC 3339 для форматування дати у міжнародному форматі.

## Дивіться також

- [Haskell Date and Time Cookbook](https://hackage.haskell.org/package/time-1.9.3/docs/Data-Time-Format.html)
- [Haskell `formatTime` function](https://www.haskell.org/hoogle/?hoogle=formatTime)
- [ISO 8601 standard](https://www.iso.org/iso-8601-date-and-time-format.html)
- [RFC 3339 standard](https://tools.ietf.org/html/rfc3339)