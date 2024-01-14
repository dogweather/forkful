---
title:    "Haskell: Отримання поточної дати"
keywords: ["Haskell"]
---

{{< edit_this_page >}}

## Чому

Багато програмирувальних завдань залежать від поточної дати - від створення програм з нагадуванням про дні народження до ведення журналів трансакцій. Отримання актуальної дати є важливою складовою будь-якої програми, і ця стаття допоможе вам зрозуміти, як зробити це в мові Haskell.

## Як

Існують декілька способів отримання поточної дати в Haskell, але ми розглянемо два найпоширеніші способи: за допомогою бібліотеки `time` та за допомогою функції `getCurrentTime`.

```Haskell
import Data.Time
import Data.Time.Clock (UTCTime)

-- Використання бібліотеки time
main = do
    now <- getCurrentTime
    let currentDate = utctDay now
    print currentDate

-- За допомогою функції getCurrentTime
main = do
    now <- getCurrentTime
    print now
```

У першому прикладі ми використовуємо бібліотеку `time`, щоб отримати дату, а у другому - за допомогою функції `getCurrentTime`. Обидва способи повертають об'єкт типу `UTCTime`, який містить як дату, так і час. Якщо вам потрібно отримати лише дату, ви можете використовувати функцію `utctDay` для отримання лише частини дати.

## Занурення в глибини

Якщо ви докладніше досліджите бібліотеку `time`, ви знайдете багато цікавих речей, які можна зробити з датами. Наприклад, ви можете обчислити різницю між двома датами, використовуючи функцію `diffDays`.

```Haskell
import Data.Time
import Data.Time.Clock (UTCTime)

-- Обчислення різниці між двома датами
date1 = fromGregorian 2021 05 22
date2 = fromGregorian 2021 07 15
diff = diffDays date2 date1
print diff -- виводить 54
```

Також, ви можете форматувати дату за допомогою функцій `formatTime` та `parseTimeM`. Ці функції дозволяють вам змінювати формат дати, а також працювати з різними часовими зонами.

## Дивіться також

- [Документація бібліотеки `time`](https://hackage.haskell.org/package/time/docs/Data-Time.html)
- [Приклади роботи з датами в Haskell](https://www.schoolofhaskell.com/school/starting-with-haskell/libraries-and-frameworks/time)