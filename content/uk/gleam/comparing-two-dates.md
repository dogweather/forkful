---
title:                "Порівняння двох дат."
html_title:           "Gleam: Порівняння двох дат."
simple_title:         "Порівняння двох дат."
programming_language: "Gleam"
category:             "Gleam"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/uk/gleam/comparing-two-dates.md"
---

{{< edit_this_page >}}

## Чому: Порівняння двох дат може бути корисним для визначення різниці в часі або перевірки, яка дата раніша чи пізніша.

## Як: Кодування прикладів та приклад виводу в межах блоків коду "```Gleam ... ```".

Найпростіший спосіб порівняти дві дати в Gleam - скористатися вбудованою функцією `DateTime.diff`. Передамо дві дати у `DateTime.diff` та вказуємо бажаний формат різниці, наприклад `:days`.

```Gleam
import Gleam.DateTime

let date1 = DateTime.from_string("2020-01-01")
let date2 = DateTime.from_string("2020-01-05")

let diff = DateTime.diff(date1, date2, :days) // diff буде рівним -4
```

Для перевірки, чи дата є ранішою або пізнішою, можна скористатися операторами порівняння `>`, `<`, `>=`, `<=`.

```Gleam
import Gleam.DateTime

let date1 = DateTime.from_string("2020-01-01")
let date2 = DateTime.from_string("2020-01-05")

if date1 > date2 {
  // код, який виконується, якщо date1 пізніша за date2
}
```

## Глибоке занурення

У файлі документації Gleam можна знайти більш детальну інформацію про роботу з датами та часом. Поміж іншим, там можна дізнатися про доступні формати і їх значення.

## Див. також

- [Документація Gleam про роботу з датами і часом](https://gleam.run/documentation/current/DateTime.html)
- [Блогпост про роботу з датами і часом в Gleam](https://gleam.run/news/dates-and-time/)