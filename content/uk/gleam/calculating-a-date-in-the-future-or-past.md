---
title:                "Обчислення дати у майбутньому або минулому"
date:                  2024-01-20T17:31:30.047492-07:00
model:                 gpt-4-1106-preview
simple_title:         "Обчислення дати у майбутньому або минулому"
programming_language: "Gleam"
category:             "Gleam"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/uk/gleam/calculating-a-date-in-the-future-or-past.md"
---

{{< edit_this_page >}}

## What & Why? (Що і Чому?)
Обчислення дати у майбутньому або минулому — це визначення точної дати, враховуючи певний проміжок часу. Програмісти використовують це для розкладів, термінів дії, і періодичних завдань.

## How to: (Як це зробити:)
```gleam
import gleam/calendar.{Date, add_days}

pub fn main() {
  let today = Date(year: 2023, month: 4, day: 7)
  let future_date = add_days(today, 10)
  let past_date = add_days(today, -10)

  future_date // Виведення: #(Date(year: 2023, month: 4, day: 17))
  past_date // Виведення: #(Date(year: 2023, month: 3, day: 28))
}
```

## Deep Dive (Занурення у деталі):
Робота з датами має давнє походження, що йде з потреби вести календарі та облік часу. У контексті програмування, це часто стосується оперування з часовими відмітками (timestamps) та періодами. 

Альтернативи для обчислення дат: 

- Використання зовнішніх бібліотек.
- Формули засновані на мілісекундах (UNIX часі).

Розрахунок дати вперед та назад за Гринвічем є достатньо тривіальним завданням, але ускладнення можуть включати врахування часових поясів, роботу з високосними роками та перехід на літній/зимовий час.

## See Also (Дивіться також):
- [Wikipedia page on Gregorian Calendar](https://uk.wikipedia.org/wiki/Григоріанський_календар)
- [ISO 8601 Date and Time Standards](https://www.iso.org/iso-8601-date-and-time-format.html)
