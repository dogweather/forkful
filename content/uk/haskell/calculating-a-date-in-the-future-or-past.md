---
title:                "Розрахунок дати в майбутньому або минулому"
html_title:           "Haskell: Розрахунок дати в майбутньому або минулому"
simple_title:         "Розрахунок дати в майбутньому або минулому"
programming_language: "Haskell"
category:             "Haskell"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/uk/haskell/calculating-a-date-in-the-future-or-past.md"
---

{{< edit_this_page >}}

## Що і чому?

Розрахунок дати у майбутньому або минулому полегшує організацію подій і спланування. Програмісти обчислюють це, щоб відслідковувати дати оплати, визначати терміни доставки, нагадування і так далі.


## Як:

Використовуючи бібліотеку Time в Haskell, ми можемо легко обчислити дату в майбутньому або минулому. Наприклад:

```Haskell
import Data.Time

main :: IO ()
main = do
    currentTime <- getCurrentTime
    let tenDays = fromIntegral (86400 * 10) :: NominalDiffTime
    print $ addUTCTime tenDays currentTime
```
При виконанні цього коду, ви побачите дату через 10 днів від поточного часу.

## Поглиблений підхід:

1. Історичний контекст: розрахунок дати в майбутньому або минулому має довгу історію в програмуванні. Однак Haskell зробив це надзвичайно простим завдяки своїй бібліотеці Time.

2. Альтернативи: У Haskell є інші бібліотеки, такі як date-arithmetic, для роботи з датами. Вибір бібліотеки залежить від вашої задачі. 

3. Деталі реалізації: Час в Haskell використовує тип `NominalDiffTime` для збереження різниці між двома моментами часу. `addUTCTime` приймає `NominalDiffTime` і `UTCTime` і повертає новий `UTCTime`.

## Див. також:

1. [Документація Haskell Time](https://hackage.haskell.org/package/time-1.6/docs/Data-Time-Clock.html).

2. [Туторіал по Data.Time бібліотеці](http://chrisdone.com/posts/haskell-time).

3. [Бібліотека date-arithmetic](https://hackage.haskell.org/package/date-arithmetic).