---
title:                "Перетворення дати в рядок"
aliases:
- /uk/rust/converting-a-date-into-a-string.md
date:                  2024-01-20T17:37:30.523361-07:00
model:                 gpt-4-1106-preview
simple_title:         "Перетворення дати в рядок"

tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/uk/rust/converting-a-date-into-a-string.md"
---

{{< edit_this_page >}}

## Що і Навіщо?
Перетворення дати в рядок - це процес у якому форматуємо дати так, як нам треба для зберігання чи виводу. Програмісти це роблять для полегшення читання дат людьми та для їх коректної обробки комп'ютерними системами.

## Як зробити:
```Rust
use chrono::{DateTime, Utc, Local, NaiveDateTime};

fn main() {
    // Час в UTC
    let utc_now: DateTime<Utc> = Utc::now();
    println!("{}", utc_now.format("%Y-%m-%d %H:%M:%S").to_string());

    // Місцевий час
    let local_now: DateTime<Local> = Local::now();
    println!("{}", local_now.format("%Y-%m-%d %H:%M:%S").to_string());

    // Без конкретної часової зони
    let naive_now: NaiveDateTime = Local::now().naive_local();
    println!("{}", naive_now.format("%Y-%m-%d %H:%M:%S").to_string());
}
```
Вивід буде приблизно таким (залежить від поточного часу):
```
2023-03-15 20:45:32
2023-03-15 23:45:32
2023-03-15 23:45:32
```

## Підводні камені:
Колись програмісти писали код для перетворення дати в рядок самостійно, але це призвело до багатьох помилок. Сучасні мови, як Rust, надають бібліотеки, такі як `chrono`, для безпечного та зручного маніпулювання датами. В `chrono` є можливість використання часових зон, настройки формату й багато чого іншого.

Альтернативи вбудованим функціям перетворення дати в Rust включають використання crates, таких як `time` або `date`. Кожний має свої переваги та недоліки, вибір залежить від конкретних вимог проекту.

Внутрішня реалізація `chrono` використовує свої типи даних для зберігання дати і часу: `DateTime`, `NaiveDateTime`, і ін. Кожен з цих типів слід використовувати в залежності від ситуації, наприклад, `DateTime` для дат із часовими зонами, і `NaiveDateTime` коли зона не важлива.

## Дивіться також:
- [Документація chrono](https://docs.rs/chrono/)
- [Стандартна бібліотека Rust: модуль time](https://doc.rust-lang.org/std/time/)
