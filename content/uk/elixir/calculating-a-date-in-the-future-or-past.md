---
title:                "Розрахунок дати у майбутньому або минулому"
html_title:           "Elixir: Розрахунок дати у майбутньому або минулому"
simple_title:         "Розрахунок дати у майбутньому або минулому"
programming_language: "Elixir"
category:             "Elixir"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/uk/elixir/calculating-a-date-in-the-future-or-past.md"
---

{{< edit_this_page >}}

## Чому
Існує багато ситуацій, коли потрібно обчислити дату в майбутньому або минулому. Наприклад, для планування подій, роботи з датами з народження або заходи, пов'язані з періодами часу. Використання Elixir для цих обчислень дає можливість легко та ефективно працювати з датами.

## Як
Одним із варіантів роботи з датами в Elixir є використання модуля `Calendar` та його функції `Date`. Цей модуль надає можливість доступу до різних операцій з датами, зокрема обчислення дати в майбутньому або минулому. Нижче наведені приклади коду та результати виконання:

```Elixir
iex> Calendar.Date.add(~D[2021-01-01], 30)
~D[2021-01-31]
```
```Elixir
iex> Calendar.Date.subtract(~D[2021-01-01], 10)
~D[2020-12-22]
```

## Глибоке занурення
Модуль `Calendar` має багато функцій для роботи з датами, наприклад перетворення дати в різні формати, розрахунок кількості днів між датами та багато іншого. Більш докладну інформацію можна знайти у [документації Elixir](https://hexdocs.pm/elixir/Calendar.html#content) та [курсі з Elixir](https://elixirschool.com/uk/lessons/specifics/dates-times/) на Elixir School.

## Дивіться також
- [Документація Elixir Calendar](https://hexdocs.pm/elixir/Calendar.html#content)
- [Курс Elixir School з роботи з датами](https://elixirschool.com/uk/lessons/specifics/dates-times/)