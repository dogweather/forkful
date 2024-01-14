---
title:    "Haskell: Перетворення дати у рядок."
keywords: ["Haskell"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/uk/haskell/converting-a-date-into-a-string.md"
---

{{< edit_this_page >}}

## Чому

Написання програм на Haskell може бути цікавим і викликати багато питань у багатьох програмістів. Одним з цікавих завдань є преобразування дати в рядок. Це допоможе вам зробити ваш код більш зрозумілим і зручнішим для використання.

## Як це зробити

Конвертування дати в рядок можливо за допомогою функції `show`, яка приймає дату і повертає її у вигляді рядка. Ось приклад коду:

```Haskell
import Data.Time.Format
import Data.Time.LocalTime

-- Форматуємо дату і час
date <- getCurrentTime
let formattedDate = formatTime defaultTimeLocale "%m/%d/%Y" date

-- Виводимо результат
print (show formattedDate)
```

Цей код з конвертує поточну дату у форматі `місяць/день/рік` і виводить її у вигляді рядка.

## Глибоке поглиблення

У Haskell є багато різних функцій для роботи з датами і часом, які можуть бути корисні для конвертування їх у різні формати. Наприклад, функція `parseTimeOrError` може бути використана для перетворення рядка у дату за заданим форматом. Також, існує багато різноманітних форматів, які можна використовувати для конвертування.

## Дивись також

- [Haskell документація з роботою з датами](https://www.haskell.org/documentation/)
- [Приклади роботи з датами у Haskell](https://wiki.haskell.org/Dates_and_times)
- [Data.Time бібліотека для роботи з датами і часом у Haskell](https://hackage.haskell.org/package/time)