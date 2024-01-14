---
title:                "Haskell: Розрахунок дати в майбутньому чи минулому."
simple_title:         "Розрахунок дати в майбутньому чи минулому."
programming_language: "Haskell"
category:             "Haskell"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/uk/haskell/calculating-a-date-in-the-future-or-past.md"
---

{{< edit_this_page >}}

## Чому

Обчислення дат в майбутньому або минулому може бути корисним для планування подій або дослідження часових рядів.

## Як

```Haskell
import Data.Time.Calendar

-- Обчислення дат в майбутньому
futureDate :: Integer -> Day
futureDate days = addDays days today

-- Обчислення дат в минулому
pastDate :: Integer -> Day
pastDate days = addDays (-days) today
```

Приклади використання функцій `futureDate` і `pastDate`:

```Haskell
today = 2021-09-03 -- поточна дата

futureDate 7 -- 2021-09-10
pastDate 14 -- 2021-08-20
```

За допомогою функцій `addDays` та `today` з модуля `Data.Time.Calendar` ми можемо легко обчислювати дати в майбутньому та минулому.

## Глибше в бік

Крім функцій для обчислення дат в майбутньому та минулому, Haskell також має інші корисні функції для роботи з датами.

Наприклад, функція `diffDays` дозволяє обчислювати різницю в днях між двома датами:

```Haskell
import Data.Time.Calendar

diffDays :: Day -> Day -> Int
```

Існує також модуль `Data.Time.Clock` для роботи з більш точною часовою інформацією та функціями для зчитування та форматування дат.

Тепер, коли ви розумієте основи використання дат в Haskell, ви можете додавати цю функціональність до своїх програм та досліджень.

## Дивіться також

- [Офіційна документація Haskell для модуля Data.Time](https://hackage.haskell.org/package/time/docs/Data-Time.html)
- [Стаття про роботу з датами в Haskell](https://dev.to/vimukthi/dealing-with-dates-and-time-in-haskell-5cnc)
- [Приклади використання дат в Haskell на сайті Stack Overflow](https://stackoverflow.com/questions/16117467/haskell-get-current-date/16117508)