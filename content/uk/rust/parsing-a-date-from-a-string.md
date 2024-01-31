---
title:                "Аналіз дати з рядка"
date:                  2024-01-20T15:38:36.050258-07:00
html_title:           "Arduino: Аналіз дати з рядка"
simple_title:         "Аналіз дати з рядка"

category:             "Rust"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/uk/rust/parsing-a-date-from-a-string.md"
---

{{< edit_this_page >}}

## Що це таке & навіщо?
Парсинг дати з рядка — це процес перетворення текстових даних у специфічний формат дати, який програма може інтерпретувати і використовувати. Програмісти роблять це, щоб легко маніпулювати датами та часом для порівняння, сортування або арифметичних операцій.

## Як це зробити:
Розглянемо код використання `chrono`, яка є популярною бібліотекою для роботи з датою та часом у Rust:

```Rust
use chrono::NaiveDate;

fn main() {
    let date_str = "2023-03-14";
    match NaiveDate::parse_from_str(date_str, "%Y-%m-%d") {
        Ok(date) => println!("Parsed date is: {}", date),
        Err(e) => println!("Error parsing date: {}", e),
    }
}
```

Вивід буде:
```
Parsed date is: 2023-03-14
```

## Поглиблений огляд:
Historically, handling dates and times is tricky because of different formats and timezones. `chrono` simplifies this in Rust. Alternatives to `chrono` include using the standard library's `time` module, though it's less feature-rich. When parsing dates, you specify the format with format specifiers like `%Y-%m-%d`. This tells the parser to expect a four-digit year, a two-digit month, and a two-digit day. The under-the-hood details involve validating format and checking calendar correctness.

## Дивіться також:
- Official `chrono` crate documentation: [docs.rs/chrono](https://docs.rs/chrono/)
- Rust `time` module documentation: [doc.rust-lang.org/std/time/index.html](https://doc.rust-lang.org/std/time/index.html)
