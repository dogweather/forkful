---
title:                "Розбір дати з рядка"
date:                  2024-02-03T19:16:07.582944-07:00
model:                 gpt-4-0125-preview
simple_title:         "Розбір дати з рядка"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/uk/rust/parsing-a-date-from-a-string.md"
changelog:
  - 2024-02-03, gpt-4-0125-preview, translated from English
---

{{< edit_this_page >}}

## Що і Чому?

Розбір дати зі строки є поширеним завданням при роботі з вхідними даними користувача або читанням даних з файлів, що включає перетворення строкових даних в формат дати, визнаний мовою програмування. У Rust це є важливим для операцій з датами, таких як порівняння, арифметика або форматування, і це покращує валідацію даних та інтегрітет у додатках.

## Як це зробити:

### Використовуючи Стандартну Бібліотеку Rust (`чейт chrono`)
Стандартна бібліотека Rust не включає прямого розбору дат, але широко використовуваний чейт `chrono` є надійним рішенням для маніпуляцій з датою та часом. Спочатку додайте `chrono` до вашого `Cargo.toml`:

```toml
[dependencies]
chrono = "0.4"
```

Потім використовуйте `chrono` для розбору строки дати в об'єкт `NaiveDate`:

```rust
extern crate chrono;
use chrono::NaiveDate;

fn main() {
    let date_str = "2023-04-01";
    let date = NaiveDate::parse_from_str(date_str, "%Y-%m-%d")
        .expect("Помилка розбору дати");

    println!("Розібрана дата: {}", date);
}

// Приклад виводу:
// Розібрана дата: 2023-04-01
```

### Використовуючи Розширене Оброблення Дати-Часу в Rust (`чейт time`)
Для більш розширеного оброблення дати-часу, включаючи більш ергономічний розбір, розгляньте чейт `time`. Спочатку включіть його до вашого `Cargo.toml`:

```toml
[dependencies]
time = "0.3"
```

Потім розберіть строку дати використовуючи тип `Date` та `PrimitiveDateTime`:

```rust
use time::{Date, PrimitiveDateTime, macros::datetime};

fn main() {
    let date_str = "2023-04-01 12:34:56";
    let parsed_date = PrimitiveDateTime::parse(
        date_str, 
        &datetime!("%Y-%m-%d %H:%M:%S")
    ).expect("Помилка розбору дати та часу");

    println!("Розібрана дата-час: {}", parsed_date);
}

// Приклад виводу:
// Розібрана дата-час: 2023-04-01 12:34:56
```

Обидва приклади демонструють, як Rust, за допомогою сторонніх чейтів, сприяє розбору строк дат в маніпульовані об'єкти дат, роблячи його потужним інструментом для розробки програмного забезпечення, що включає тимчасові дані.