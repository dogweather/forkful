---
title:                "Порівняння двох дат"
html_title:           "Clojure: Порівняння двох дат"
simple_title:         "Порівняння двох дат"
programming_language: "Haskell"
category:             "Haskell"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/uk/haskell/comparing-two-dates.md"
---

{{< edit_this_page >}}

## Що це і навіщо?

Порівняння двох дат – це процес визначення яка дата є більш ранньою або пізньою. Програмісти це роблять, щоб впорядковувати події в часі, визначати тривалість періодів або автоматизувати обробку дедлайнів.

## Як це робити:

```Haskell
import Data.Time.Clock
import Data.Time.Calendar

main = do
    let date1 = fromGregorian 2020 9 16
    let date2 = fromGregorian 2020 10 16
    print (date1 < date2)
```

Вивід:

```Haskell
True
```

У цьому прикладі ми порівнюємо 16 вересня 2020 року та 16 жовтня 2020 року. Результат "True" означає, що вересень був раніше, ніж жовтень.

## Погрузимося глибше:

### Історичний контекст:

Haskell використовує дату Грегоріанського календаря, який є найбільш поширеним в світі. На відміну від інших типів дат, таких як Unix-час, час в Haskell витримує перевірку часом і не має проблеми року 2038.

### Альтернативи:

Альтернативою є використання бібліотеки time, яка містить типи для роботи зі складнішими форматами дати і часу.

### Технічні подробиці:

При порівнянні дат в Haskell використовується визначення порядку, яке вбудоване в типі даних. Функція fromGregorian створює дату, а оператор (<) використовується для порівняння.

## Зверніть увагу також:

1. [Офіційна документація Haskell по часу](https://hackage.haskell.org/package/time-1.9.3/docs/Data-Time-Clock.html)
3. [Вступ до вбудованого порядку в Haskell](http://learnyouahaskell.com/making-our-own-types-and-typeclasses#the-algebraic-in-lists)