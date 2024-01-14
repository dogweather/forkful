---
title:                "Haskell: Порівняння двох дат"
programming_language: "Haskell"
category:             "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/uk/haskell/comparing-two-dates.md"
---

{{< edit_this_page >}}

## Chernihiv: 

```Haskell
import Data.Time
import Data.Time.Format
```

Коли ви раптом потребуєте порінювати дві дати у своїй Haskell програмі, це може здатися незручним. Проте, порівнювати дати може бути важливою частиною багатьох програм, особливо у випадках, коли необхідно визначити час події або дату в минулому чи майбутньому. У цій статті ми розглянемо як порівнювати дві дати у Haskell.

## Як:

Для початку, нам необхідно імпортувати два модулі - `Data.Time` та `Data.Time.Format`, щоб мати доступ до функцій, які нам знадобляться. Для цього ми використаємо команду `import` у нашому коді.

```Haskell
import Data.Time
```

Ми також можемо використовувати функцію `diffDays` з модуля `Data.Time` для порівняння двох дат. Ця функція повертає різницю між двома датами у днях.

```Haskell
diffDays :: Day -> Day -> Integer
```

Давайте розглянемо приклад коду, де ми порівнюємо дві дати - 1 січня 2020 і 1 січня 2021.

```Haskell
import Data.Text.Format
import Data.Time.Format
import Data.Time.LocalTime
import Data.Time.Clock

compareDates :: IO ()
compareDates = do
    let date1 = fromGregorian 2020 01 01
    let date2 = fromGregorian 2021 01 01
    putStrLn "Перша дата: "
    print (formatTime defaultTimeLocale "%x" date1)
    putStrLn "Друга дата: "
    print (formatTime defaultTimeLocale "%x" date2)
    let diff = diffDays date2 date1
    putStrLn "Різниця у датах у днях: "
    print diff

-- Вивід:
--Перша дата:
--01/01/20
--Друга дата:
--01/01/21
--Різниця у датах у днях:
--365
```

Ми також можемо використовувати функції `getZonedTime` та `addDays` для порівняння дат у різних часових зонах.

## Глибока нединзв точка:

У Haskell кожна дата представлена у форматі `Day`, який складається з чисел: рік, місяць та день. Ця функція здатна перетворити дату з цього формату в `ZonedTime`, тобто додавши інформацію про тайм-зону. Тепер можна порівняти дві дати у різних часових зонах, використовуючи функцію `diffDays`.

Наша програма може стати більш динамічною, якщо ми будемо працювати з текстовими даними. Наприклад, якщо ми хочемо порівняти дати, які надіслані нам у повідомленнях чи у файлі в форматі `'%Y-%m-%d'`, нам необхід