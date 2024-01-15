---
title:                "Отримання поточної дати."
html_title:           "Haskell: Отримання поточної дати."
simple_title:         "Отримання поточної дати."
programming_language: "Haskell"
category:             "Haskell"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/uk/haskell/getting-the-current-date.md"
---

{{< edit_this_page >}}

## З чого почати: отримання поточної дати в Haskell

Об'єктивно кажучи, отримання поточної дати є необхідною операцією для багатьох Haskeller'ів. Незалежно від того, чи ви працюєте над проектом, який використовує часові мітки, або просто хочете додати корисну функцію до своєї програми, знання, як отримати поточну дату в Haskell, буде корисним для вас.

## Кодуємо у Haskell

```Haskell
import Data.Time.Clock (getCurrentTime, utctDay)
import Data.Time.Calendar (Day, showGregorian)

-- Отримуємо поточну дату
currentDate :: IO Day
currentDate = do
    time <- getCurrentTime
    return $ utctDay time

main = do
    date <- currentDate
    putStrLn $ "Сьогодні " ++ showGregorian date

-- Результат
-- "Сьогодні 2020-09-23"
```

Завдяки модулю `Data.Time.Clock` та функції `getCurrentTime`, ми можемо отримати поточний час у вигляді `UTCTime`. Щоб перетворити його на більш зрозумілий формат, який як правило називається `Day`, ми використовуємо функцію `utctDay`.

У нашому прикладі, ми використовуємо функцію `showGregorian` для виведення дати у зрозумілому форматі року-місяць-день. За допомогою цього прикладу ви можете отримати будь-які потрібні дані про поточну дату, як-от день тижня або кількість днів у місяці.

## Глибше в деталі

Отримання поточної дати в Haskell за допомогою функції `getCurrentTime` є досить простим і широко використовується в різних програмах. Однак, ви повинні бути уважні при роботі з датами у своїй програмі, оскільки часові зони та формати дат можуть відрізнятися в різних місцях світу та залежати від налаштувань вашої операційної системи.

## Дивіться також

- [Офіційна документація з модуля `Data.Time.Clock`](https://hackage.haskell.org/package/time-1.9.3/docs/Data-Time-Clock.html)
- [Стаття про роботу з датами в Haskell](https://www.fpcomplete.com/blog/2017/12/working-with-dates-in-haskell/)