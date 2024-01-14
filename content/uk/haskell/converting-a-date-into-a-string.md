---
title:                "Haskell: Перетворення дати в рядок"
simple_title:         "Перетворення дати в рядок"
programming_language: "Haskell"
category:             "Haskell"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/uk/haskell/converting-a-date-into-a-string.md"
---

{{< edit_this_page >}}

## Чому

У програмуванні мовою Haskell, часто виникає необхідність конвертувати дату в рядок для подальшої обробки або виводу на екран. Це може бути корисно для створення звітів, роботи з базами даних чи просто для зручності користувачів. У цій статті ми дізнаємося, як це зробити.

## Як це зробити

Для початку, нам потрібно визначити тип даних Date, щоб мати змогу працювати з датами. Для цього використаємо модуль Data.Time і функцію ```Day```:
  
```Haskell
import Data.Time
date :: Day
```

Далі, ми можемо використати функцію ```formatTime```, яка дозволяє конвертувати дату в рядок за заданим шаблоном.

```Haskell
import Data.Time
formatTime :: Day -> String -> String
```

Перший аргумент - це дата, яку ми хочемо сконвертувати, а другий - шаблон, за яким будуть виводитися дані. Наприклад, щоб отримати рядок у форматі день/місяць/рік, ми можемо використати шаблон "%d/%m/%Y".

```Haskell
import Data.Time
date :: Day
date = fromGregorian 2021 04 15
formatTime date "%d/%m/%Y"
-- вивід: "15/04/2021"
```

Також за допомогою функції ```parseTimeM```, ми можемо конвертувати рядок в дату за заданим шаблоном.

```Haskell
import Data.Time
parseTimeM :: Monad m => TimeLocale -> String -> String -> m Day
```

Ця функція приймає три аргументи: TimeLocale, який описує формат дати; шаблон рядка, за яким потрібно проводити конвертацію; і сам рядок, який потрібно конвертувати.

```Haskell
import Data.Time
parseTimeM defaultTimeLocale "%d/%m/%Y" "15/04/2021" :: Maybe Day
-- вивід: Just 2021-04-15
```

## Глибоке занурення

У модулі Data.Time також є багато інших функцій для роботи з датами, таких як отримання дня тижня, розрахунок різниці між двома датами та інші. Для детальнішого ознайомлення з ними, рекомендуємо ознайомитися з документацією по модулю.

## Дивись також

- [Офіційна документація Data.Time](https://hackage.haskell.org/package/time/docs/Data-Time.html)
- [Створення та конвертування дат в Haskell](https://serokell.io/blog/haskell-dates)
- [Різноманітні формати дат і часу в Haskell](https://www.ircmaxell.com/blog/2014/11/what-every-developer-should-know-about-dates-and-times.html)