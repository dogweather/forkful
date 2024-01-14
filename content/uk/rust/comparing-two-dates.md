---
title:                "Rust: Порівняння двох дат"
simple_title:         "Порівняння двох дат"
programming_language: "Rust"
category:             "Rust"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/uk/rust/comparing-two-dates.md"
---

{{< edit_this_page >}}

## Чому

Розробка програм на Rust може бути викликом для багатьох розробників, особливо для початківців. Але вміння порівнювати дві дати є корисним і необхідним навичкою для багатьох проєктів. В цій статті ми розглянемо як порівнювати дві дати за допомогою Rust і зрозуміємо чому це є важливим.

## Як

Для порівняння двох дат в Rust нам потрібно використати модуль `chrono`, що вже включений у стандартну бібліотеку. Почнемо з встановлення необхідного модулю:

```Rust
extern crate chrono;

use chrono::{NaiveDate, Duration};

fn main() {
    // створюємо дві дати
    let date1 = NaiveDate::from_ymd(2021, 7, 20);
    let date2 = NaiveDate::from_ymd(2020, 7, 20);

    // порівнюємо їх
    let comparison = date1.cmp(&date2);

    // виводимо результат в консоль
    println!("Результат порівняння: {:?}", comparison);
}
```

Вивід:

```
Результат порівняння: Greater
```

Також, можна використати функцію `Duration::days()` для визначення різниці між датами у днях:

```Rust
let difference = date1.signed_duration_since(date2).num_days();
```

## Глибоке дослідження

Можливості модуля `chrono` дозволяють здійснювати більш складні порівняння, наприклад, перевіряти чи дати належать до одного і того ж місяця або року, знаходити найближчу до поточної дату і т.д. Також, варто звернути увагу на форматування дат для більш зручного використання у коді.

## Дивіться також

- [Документація по модулю `chrono`](https://docs.rs/chrono)
- [Вступ до програмування на Rust](https://rust-lang.org/uk/learn/get-started)
- [Приклади використання модуля `chrono`](https://github.com/chronotope/chrono/tree/master/examples)