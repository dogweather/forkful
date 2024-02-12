---
title:                "Обчислення дати у майбутньому або минулому"
aliases:
- /uk/haskell/calculating-a-date-in-the-future-or-past.md
date:                  2024-01-20T17:31:42.768859-07:00
model:                 gpt-4-1106-preview
simple_title:         "Обчислення дати у майбутньому або минулому"

tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/uk/haskell/calculating-a-date-in-the-future-or-past.md"
---

{{< edit_this_page >}}

## Що та Чому?
Обчислення дати у майбутньому чи минулому — це процес знаходження дат, що відрізняються від вихідної на заданий період. Програмісти роблять це для планування задач, прогнозування подій, чи обробки дат.

## Як це зробити:
Гарно, напишемо функцію. Вас знайомити з `Data.Time` бібліотекою.

```Haskell
import Data.Time

addDaysToCurrent :: Integer -> IO Day
addDaysToCurrent n = do
  currentDay <- utctDay <$> getCurrentTime
  return $ addDays n currentDay

main :: IO ()
main = do
  putStrLn "Сьогодні плюс 10 днів буде:"
  tenDaysLater <- addDaysToCurrent 10
  print tenDaysLater
```

Запустимо код і отримаємо:
```
Сьогодні плюс 10 днів буде:
2023-04-21
```

## Поглиблений розгляд:
Обчислення дати було завжди актуальним. Астрономи, історики й банкіри — всі працювали з календарями. В Haskell, `Data.Time` це стандартна бібліотека для роботи з часом. Вона охоплює часові зони, і на відміну від старої `Data.Time.Calendar`, враховує переведення часу. Альтернативи — `time` і `old-time`, але `Data.Time` використовувати простіше і воно має повнішу підтримку таймзон.

## Дивіться також:
- [Haskell Data.Time Library](https://hackage.haskell.org/package/time-1.9.3/docs/Data-Time.html)
- [Learn You a Haskell for Great Good! (by Miran Lipovača)](http://learnyouahaskell.com/) - Легкий вступ до Haskell.

Тримайтеся просто, глибоко розумійте свої інструменти, і програмуйте щасливо!
