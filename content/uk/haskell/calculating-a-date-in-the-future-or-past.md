---
title:    "Haskell: Обчислення дати у майбутньому або минулому"
keywords: ["Haskell"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/uk/haskell/calculating-a-date-in-the-future-or-past.md"
---

{{< edit_this_page >}}

## Чому

Урахування дати у майбутньому або минулому може бути корисним для багатьох програмних задач, наприклад, створення розкладу подій або обробка платіжних дат.

## Як

```Haskell
import Data.Time

-- код для отримання дати за 1 день до поточної дати
getYesterday :: IO Day
getYesterday = do
  today <- getCurrentTime
  let yesterday = addDays (-1) (utctDay today)
  return yesterday

-- код для отримання дати через 1 рік від поточної дати
getOneYearFromNow :: IO Day
getOneYearFromNow = do
  today <- getCurrentTime
  let oneYearFromNow = addGregorianYearsClip 1 (utctDay today)
  return oneYearFromNow

-- приклад використання функцій
main :: IO ()
main = do
  yesterday <- getYesterday
  oneYearFromNow <- getOneYearFromNow
  print yesterday
  print oneYearFromNow
```

Результат виконання програми буде виглядати так:

```
2021-10-05
2022-10-04
```

## Глибше поринемо

В хаскелі існує багато корисних функцій для роботи з датами, таких як `addDays` для добавлення/віднімання днів, `addGregorianYearsClip` для добавлення/віднімання років в григоріанському календарі та багато інших. Також, для зручності, було створено бібліотеку `time`, яка містить багато корисних функцій для обробки дат. Рекомендується ознайомитись з документацією, щоб дізнатися більше про доступні функції та їх можливості.

## Дивись також

- [Документація по бібліотеці `time`](https://hackage.haskell.org/package/time)
- [Приклади використання функцій для обробки дат](https://www.schoolofhaskell.com/school/starting-with-haskell/libraries-and-frameworks/time)