---
title:    "Elixir: Обчислення дати у майбутньому або минулому"
keywords: ["Elixir"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/uk/elixir/calculating-a-date-in-the-future-or-past.md"
---

{{< edit_this_page >}}

## Чому

Розрахунок дати в майбутньому або минулому може виявитися корисним в багатьох ситуаціях, наприклад, при розробці програмного забезпечення для календаря або планування подій. Також це може бути цікаво для тих, хто просто любить експериментувати з еліксиром.

## Як

Для початку, нам потрібно імпортувати модуль `Calendar`, в якому знаходиться функція `add`, яка дозволяє додавати або віднімати дні, місяці або роки від поточної дати.

```Elixir
import Calendar

# Розрахунок дати в майбутньому
add(Calendar.utc_now, years: 2)

# Розрахунок дати в минулому
add(Calendar.utc_now, days: -5)

# Розрахунок дати з використанням дати в майбутньому як початкової
add(add(Calendar.utc_now, years: 2), days: 10)
```

Вище наведені приклади використовують поточну дату як початкову для розрахунку дати в майбутньому або минулому. Також можна вказати будь-яку іншу дату, використовуючи функцію `Date.utc_from_erl!`.

```Elixir
# Розрахунок дати в минулому від 15 січня 2020 року
add(Date.utc_from_erl!(2020, 1, 15), days: -7)
```

## Глибоке дослідження

Функція `add` використовує модуль `NaiveDateTime`, який представляє дату і час без врахування часового поясу. Якщо вам потрібно врахувати часовий пояс, можна використати модуль `DateTime`, що містить функції `to_date_time` і `from_date_time`.

```Elixir
datetime = NaiveDateTime.new(2020, 1, 15, 12, 0, 0)
DateTime.to_date_time(datetime)
# Результат: #DateTime<2020-01-15 12:00:00Z>

datetime = NaiveDateTime.new(2020, 1, 15, 12, 0, 0)
DateTime.from_date_time(datetime, "Europe/Kiev")
# Результат: #DateTime<2020-01-15 14:00:00+02:00>
```

## Дивіться також

- [Офіційна документація Elixir для модуля Calendar](https://hexdocs.pm/elixir/Calendar.html)
- [Elixir School - Робота з датою і часом](https://elixirschool.com/uk/lessons/basics/date-and-time/)
- [Програмування в Elixir - Графічний календар з початку і кінця року](https://programming-elixir.com/blog/2019/02/06/calendar-with-mix-over-year-range/)