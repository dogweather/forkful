---
title:    "Haskell: Порівняння двох дат"
keywords: ["Haskell"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/uk/haskell/comparing-two-dates.md"
---

{{< edit_this_page >}}

## Чому

У порівнянні двох дат є важливим етапом для багатьох проектів. Це дає можливість перевірити правильність обробки дат та уникнути потенційних проблем у проекті.

## Як

Перш за все, встановіть пакет `time` використовуючи менеджер пакетів Haskell, такий як `cabal` чи `stack`:

```Haskell
cabal install time
```

імпортуйте `Data.Time` для роботи з датами:

```Haskell
import Data.Time
```

Щоб порівняти дві дати, скористайтеся функцією `diffDays` та переведіть дати в тип `Day`:

```Haskell
-- Порівнюємо час, що минув від дати 01.01.2020 до 01.01.2021

-- Створюємо об'єкти типу `Day` для дат
let date1 = fromGregorian 2020 1 1 
let date2 = fromGregorian 2021 1 1 

-- Використовуємо функцію `diffDays` для порівняння дат
let diff = diffDays date2 date1 

-- Виводимо результат на екран
print diff 

-- Виведе: 366 (кількість днів, що минуло між датами)
```

## Глибокий занурення

Функція `diffDays` приймає тип `Day`, який представляє дату в Григоріанському календарі. У цьому календарі, дати подані у форматі `[рік, місяць, день]`, де рік має 4 цифри, а місяць та день мають 2 цифри.

У прикладі вище, ми використовуємо функцію `fromGregorian` для створення об'єкта типу `Day` з 3 аргументами: рік, місяць та день.

Також варто врахувати, що функція `diffDays` повертає кількість днів між двома датами, а не їх різницю.

## Дивіться також

- [Пакет `time` в Hackage](https://hackage.haskell.org/package/time)
- [Тип `Day` у модулі `Data.Time.Calendar`](https://hackage.haskell.org/package/time/docs/Data-Time-Calendar.html#t:Day)