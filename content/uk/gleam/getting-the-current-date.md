---
title:                "Отримання поточної дати"
html_title:           "Gleam: Отримання поточної дати"
simple_title:         "Отримання поточної дати"
programming_language: "Gleam"
category:             "Gleam"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/uk/gleam/getting-the-current-date.md"
---

{{< edit_this_page >}}

## Що і чому?
Отримання поточної дати - це дія, яку часто виконують програмісти для отримання поточних даних, які необхідні для виконання різних завдань. Наприклад, для запису даних за сьогоднішній день в базу даних або для створення унікальних ідентифікаторів.

## Як зробити:
У Gleam є вбудована функція для отримання поточної дати ```Gleam.datetime.now()```. Вона повертає дату та час у форматі DateTime, який можна подальше використовувати для різних операцій. Наприклад:

```Gleam
let current_date = Gleam.datetime.now()
```

Ви також можете використовувати інші методи для отримання інформації про поточну дату, такі як ```Gleam.datetime.day(current_date)```, ```Gleam.datetime.month(current_date)```, ```Gleam.datetime.year(current_date)``` і т.д.

## Огляд:
Отримання поточної дати - це важлива функція, яка використовується для багатьох різних завдань у програмуванні. Існують також альтернативні методи, які можуть бути використані для отримання дати, такі як використання зовнішніх бібліотек або перетворення дати з рядків.

Для отримання поточної дати Gleam використовує бібліотеку Erlang, що надає багато корисних функцій для роботи з датами та часом. Глибока розробка цих функцій може зайняти більш детальний аналіз в Erlang документації.

## Дивіться також:
- [Datetime - Gleam документація](https://gleam.run/documentation/stdlib/datetime/)
- [Datetime - Erlang документація](http://erlang.org/doc/man/calendar.html#id109627)
- [Erlang - Gleam документація](https://gleam.run/documentation/stdlib/erlang/)