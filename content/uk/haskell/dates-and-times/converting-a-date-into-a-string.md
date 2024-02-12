---
title:                "Перетворення дати в рядок"
aliases:
- /uk/haskell/converting-a-date-into-a-string.md
date:                  2024-01-20T17:37:02.490411-07:00
model:                 gpt-4-1106-preview
simple_title:         "Перетворення дати в рядок"

tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/uk/haskell/converting-a-date-into-a-string.md"
---

{{< edit_this_page >}}

## Що і Чому?
Перетворення дати на рядок дає можливість зберігати або відображати дату в зручному та зрозумілому форматі. Програмісти роблять це, щоб легко взаємодіяти з користувачами та іншими системами, обмінюватися даними та виконувати читабельний вивід.

## Як це зробити:
Для роботи з часом і датою в Haskell ми використовуємо пакет `time`. Спершу, імпортуйте потрібний модуль і встановіть часовий формат.

```Haskell
import Data.Time

-- Приклад: перетворення поточної дати в рядок
main :: IO ()
main = do
    currentDateTime <- getCurrentTime
    let dateString = formatTime defaultTimeLocale "%Y-%m-%d" currentDateTime
    putStrLn dateString
```

Запустивши цей код, ви отримаєте вивід у форматі "YYYY-MM-DD".

## Поглиблено:
У Haskell для роботи з датою та часом модуль `Data.Time` містить багато функцій. Історично, системи Haskell для управління часом еволюціонували, розширюючи можливості й надаючи кращу підтримку часових зон.

Альтернативами `Data.Time` є сторонні бібліотеки, такі як `time-recurrence` для управління повторюваними подіями.

Детальніше, якщо вам треба враховувати часові зони, використовуйте тип `ZonedTime`. Працювати з форматом можна за допомогою `formatTime`, де можна вказати маску для виведення як рядка. 

## Додатково:
- `time` пакет на Hackage: https://hackage.haskell.org/package/time
- Більше про час і дати в GHC: https://www.haskell.org/ghc/blog/20210304-time-1.11-release.html
- Посібник по `Data.Time.Format`: https://hackage.haskell.org/package/time-1.11/docs/Data-Time-Format.html
