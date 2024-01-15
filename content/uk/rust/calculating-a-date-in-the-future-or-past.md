---
title:                "Розрахунок дати в майбутньому або минулому"
html_title:           "Rust: Розрахунок дати в майбутньому або минулому"
simple_title:         "Розрахунок дати в майбутньому або минулому"
programming_language: "Rust"
category:             "Rust"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/uk/rust/calculating-a-date-in-the-future-or-past.md"
---

{{< edit_this_page >}}

## Чому
Навіщо потрібно обчислювати дату в майбутньому або минулому? Наприклад, це дуже корисно, якщо ви плануєте подорож або потрібно зробити розрахунки для фінансів.

## Як
Щоб обчислити дату в майбутньому або минулому використовуйте стандартну бібліотеку `chrono` в Rust. Переконайтеся, що ви маєте встановлену останню версію програмувальної мови Rust і додаток `cargo`. Далі вам потрібно підключити бібліотеку за допомогою `use chrono::prelude::*;` і використовувати функцію `Local::today()` для отримання поточної дати. Для обчислення дати в майбутньому додайте або відніміть бажану кількість днів за допомогою методів `.add()` і `.sub()`.

```Rust
use chrono::prelude::*;

fn main() {
  let today = Local::today();
  let future_date = today.add(365); // обчислюємо дату через 365 днів
  println!("Дата у майбутньому: {}", future_date);
}
```

Вивід: `Дата у майбутньому: 2022-05-27`

Для обчислення дати в минулому використовуйте метод `.sub()` та від'ємну кількість днів. 

```Rust
use chrono::prelude::*;

fn main() {
  let today = Local::today();
  let past_date = today.sub(365); // обчислюємо дату за минулий рік
  println!("Дата у минулому: {}", past_date);
}
```

Вивід: `Дата у минулому: 2020-05-28`

## Deep Dive
Якщо вам потрібно обчислити дату на певну кількість років, місяців або навіть годин, використовуйте метод `.with_period()` разом з відповідними параметрами. Наприклад, для обчислення дати через 2 роки, 3 місяці та 5 годин від поточної дати, використайте такий код:

```Rust
use chrono::prelude::*;

fn main() {
  let today = Local::today();
  let future_datetime = today.with_period(1.year() + 3.months() + 5.hours()); // обчислюємо дату
  println!("Дата з періодом: {}", future_datetime);
}
```

Вивід: `Дата з періодом: 2022-08-01 11:00:00`

## See Also
- [Official Rust Documentation](https://doc.rust-lang.org/stable/std/chrono/) 
- [Learn Rust by Example](https://rustbyexample.com/std/chrono.html)