---
title:                "Перетворення дати в рядок"
html_title:           "Haskell: Перетворення дати в рядок"
simple_title:         "Перетворення дати в рядок"
programming_language: "Haskell"
category:             "Haskell"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/uk/haskell/converting-a-date-into-a-string.md"
---

{{< edit_this_page >}}

# Що і навіщо?
Конвертування дати у рядок - це процес перетворення дати у зрозумілий для комп'ютера формат, щоб його можна було обробити та використовувати в програмі. Це корисний підхід для програмістів, які працюють з обробкою дати в своїх програмах.

# Як це зробити?
```Haskell
import Data.Time.Format (formatTime, defaultTimeLocale)
import Data.Time.Clock (getCurrentTime)
import System.Locale (defaultTimeLocale)

main :: IO()
main = do
  currentTime <- getCurrentTime
  let dateString = formatTime defaultTimeLocale "%Y-%m-%d" currentTime
  putStrLn $ "Сьогоднішня дата: " ++ dateString
```

В результаті ми отримуємо рядок, що відображає сьогоднішню дату у форматі "YYYY-MM-DD", наприклад "2020-05-05". Ми використали функції з модулів Data.Time.Format та Data.Time.Clock, а також змінну defaultTimeLocale, щоб встановити стандартний формат дати.

# Глибше процесу
Конвертування дати у рядок є важливою частиною програмування, особливо у веб-додатках та базах даних. Наприклад, при відображенні дати на інтерфейсі користувача також потрібно конвертувати його у зрозумілий формат. Альтернативою є використання бібліотек, таких як TimeSeries чи Time as Data, але вони можуть бути складні у використанні для початківців.

Імплементаційні деталі включають в себе різні формати дати та часу, підтриманих мовою Haskell, що можуть відрізнятися від інших мов програмування. Також важливо врахувати часові зони та локалі при конвертуванні дати.

# Дивись також
- [Офіційна документація з модуля Data.Time.Format](https://hackage.haskell.org/package/time-1.9.3/docs/Data-Time-Format.html)
- [Вступ до роботи з датами та часом у Haskell](https://www.schoolofhaskell.com/school/starting-with-haskell/basics-of-haskell/10-dates-and-times-in-haskell)
- [Бібліотека TimeSeries](https://hackage.haskell.org/package/timeseries)