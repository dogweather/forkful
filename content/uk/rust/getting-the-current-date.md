---
title:    "Rust: Отримання поточної дати"
keywords: ["Rust"]
---

{{< edit_this_page >}}

## Чому

Розробка програмного забезпечення є захоплюючим та важливим завданням для багатьох українських програмістів. Однак, навіть у найпростіших програмах часто потрібно отримувати поточну дату та час. Це може здатися тривіальною задачею, проте вона має свої нюанси та функції, які ми розглянемо у цій статті.

## Як це зробити

Отримання поточної дати та часу в програмі на Rust є простою та зручною задачею. Для початку, ми можемо використовувати бібліотеку `chrono`, яка є найпоширенішим інструментом для роботи з датою та часом в Rust. Нижче наведено приклад коду, який можна скопіювати та вставити у свій проект:

```rust
extern crate chrono;

use chrono::{Local, DateTime};

fn main() {
    let now: DateTime<Local> = Local::now();
    println!("Поточна дата та час: {}", now);
}
```

Також, ми можемо відобразити поточну дату та час у зручному для нас форматі, наприклад, у форматі `день-місяць-рік година:хвилина:секунда`:

```rust
extern crate chrono;

use chrono::{Local, DateTime, Datelike, Timelike};

fn main() {
    let now: DateTime<Local> = Local::now();
    let day = now.day();
    let month = now.month();
    let year = now.year();
    let hour = now.hour();
    let minute = now.minute();
    let second = now.second();
    println!("Поточна дата та час: {}-{}-{} {}:{}:{}", day, month, year, hour, minute, second);
}
```

В результаті, ми отримаємо виведення, подібне до `Поточна дата та час: 12-05-2021 15:30:28`.

## Глибоке погруження

Для трохи більш складних обчислень з датою та часом, бібліотека `chrono` має багато корисних функцій. Наприклад, її можна використовувати для порівняння двох дат або визначення різниці в часі між ними. Також, вона має підтримку різних часових поясів та може виконувати перетворення між ними. Більше інформації можна знайти у [офіційній документації бібліотеки `chrono`](https://docs.rs/chrono/latest/chrono/).

## Дивіться також

- [Офіційний сайт Rust](https://www.rust-lang.org/uk)
- [Офіційна документація бібліотеки `chrono`](https://docs.rs/chrono/latest/chrono/)
- [Чому Rust - популярна мова програмування у сучасному світі](https://itcluster.lviv.ua/why-rust-is-programming