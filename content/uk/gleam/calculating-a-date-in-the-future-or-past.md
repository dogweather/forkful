---
title:    "Gleam: Обчислення дати в майбутньому або минулому"
keywords: ["Gleam"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/uk/gleam/calculating-a-date-in-the-future-or-past.md"
---

{{< edit_this_page >}}

##Чому
Калькуляція дати в майбутньому або минулому може бути корисною для тих, хто хоче планувати події або перевіряти відповідність дати до певних подій.

##Як
```Gleam
let date_in_future = Date.add_days(Date.today(), 7)
// Output: 2022-01-17T00:00:00Z

let date_in_past = Date.sub_days(Date.today(), 14)
// Output: 2021-12-20T00:00:00Z
```

##Глибоке дослідження
Калькуляція дати в майбутньому або минулому може також здійснюватися за допомогою функцій `add_weeks()`, `add_months()` та `add_years()`. Крім того, є можливість визначити час, не лише дату, за допомогою функцій `add_seconds()`, `add_minutes()` та `add_hours()`. Також можна використовувати різні арифметичні операції для калькулювання більш складних дат.

##Дивись також
- [Офіційна документація Gleam](https://gleam.run/)
- [Документація з роботи з датами в Gleam](https://gleam.run/core-lib/Date.html)
- [Приклади використання Gleam для калькуляції дат](https://github.com/gleam-lang/gleam/blob/main/examples/dates.gleam)