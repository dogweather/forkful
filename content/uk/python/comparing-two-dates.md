---
title:                "Порівняння двох дат"
html_title:           "Clojure: Порівняння двох дат"
simple_title:         "Порівняння двох дат"
programming_language: "Python"
category:             "Python"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/uk/python/comparing-two-dates.md"
---

{{< edit_this_page >}}

## Що і чому?

Порівняння двох дат – це процес встановлення, яка дата відбулася раніше або пізніше. Програмісти це роблять для виконання операцій на основі часу, таких як відстеження подій, сортування даних за датою тощо.

## Як це зробити:

Метод `datetime` модуля `datetime` в Python використовується для представлення дати та часу.

```Python
from datetime import datetime

# Окремі дати
date1 = datetime(2021, 5, 21)
date2 = datetime(2022, 5, 22)

# Порівняти дати
if date1 < date2:
    print('date1 раніша за date2')
else:
    print('date1 не раніша за date2')
```

Вивід:

```Shell
date1 раніша за date2
```

## Поглиблено:

л) Історичний контекст: У ранніх версіях Python порівняння дат було більш складним та неефективним. Однак, з введенням модуля `datetime` у Python 2.3, процес став набагато простішим та ефективнішим.

2) Альтернативи: Інші методи порівняння дат включають використання `timedelta` або сторонніх бібліотек, таких як `dateutil`.

3) Деталі реалізації: Під час порівняння дат Python перетворює обидві дати на їхні представлення у форматі UNIX-часу (кількість секунд з полуночі 1 січня 1970 р., UTC) і порівнює ці цілочисельні значення.

## Дивіться також:

1) [Офіційна документація Python про модуль datetime](https://docs.python.org/3/library/datetime.html)
2) [Python datetime - A Primer by Real Python](https://realpython.com/python-datetime/)
3) [Python Date and Time by Programiz](https://www.programiz.com/python-programming/datetime)