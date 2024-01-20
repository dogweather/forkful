---
title:                "Отримання поточної дати"
html_title:           "Bash: Отримання поточної дати"
simple_title:         "Отримання поточної дати"
programming_language: "Haskell"
category:             "Haskell"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/uk/haskell/getting-the-current-date.md"
---

{{< edit_this_page >}}

## Що і навіщо?

Отримати поточну дату - це отримати інформацію про сьогоднішній день: рік, місяць, день. Це може бути корисним для програм, що залежать від часу: від запису логів до обробки термінів платежів.

## Як це зробити:

Отже, в Haskell ви можете зробити це за допомогою бібліотеки "time". Приклад коду:

```Haskell
import Data.Time.Clock
import Data.Time.Calendar

main = do
    currentDay <- getCurrentTime
    print $ utctDay currentDay
```

Це поверне поточну дату в форматі `YYYY-MM-DD`.

## Занурюємося глибше:

Haskell використовує типи даних UTCTime і Day для роботи з датами. UTCTime є кількістю секунд від определеній дати і часу (точка відліку), а Day може представляти будь-який день в новому стилі (після 15.10.1582). 

Як альтернативу, ви можете використовувати LocalTime з Data.Time.LocalTime. Для перетворення між UTCTime і LocalTime використовується TimeZone.

Деталі реалізації цих модулів дуже цікаві, але вони включають велику кількість математики і теорії часу і можуть бути складними для початківців.

## Див. також:

- [Data.Time вікі Hackage](http://hackage.haskell.org/package/time-1.6.0.1/docs/Data-Time.html)
- [Робота з датами і часом в Haskell, Стаковерфлоу](https://stackoverflow.com/questions/2408976/haskell-datetime-library)
- [Огляд моделі часу в Haskell, Лі Суї Йенсен](https://byorgey.wordpress.com/2009/12/22/beginnings-of-a-time-library/)