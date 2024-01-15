---
title:                "Порівняння двох дат"
html_title:           "Rust: Порівняння двох дат"
simple_title:         "Порівняння двох дат"
programming_language: "Rust"
category:             "Rust"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/uk/rust/comparing-two-dates.md"
---

{{< edit_this_page >}}

## Чому

Реальне життя складається з багатьох подій, всі вони мають свої дати. Порівняння двох дат може бути корисним при роботі зі строками, термінами виконання або знаходженні проміжків часу між подіями.

## Як

Перш за все, давайте імпортуємо стандартну бібліотеку для роботи з датами:

```Rust
use std::time::{Duration, SystemTime};
```

Для порівняння двох дат використовуємо метод `cmp`:

```Rust
let date1 = SystemTime::now();
let date2 = SystemTime::now() + Duration::from_secs(3600); // Додати 1 годину до поточної дати

match date1.cmp(&date2) {
    Ordering::Less => println!("Дата 1 раніше за датою 2"),
    Ordering::Greater => println!("Дата 1 пізніше за датою 2"),
    Ordering::Equal => println!("Обидві дати однакові"),
}
```

Ви можете також порівнювати дати з використанням методів `eq`, `ne`, `gt`, `lt`, `ge`, `le`. Детальніше про ці методи та синтаксис порівняння дат можна знайти у документації [Rust](https://doc.rust-lang.org/std/time/struct.SystemTime.html).

## Deep Dive

В Rust дати представлені структурою `SystemTime`, яка зберігає кількість секунд від 00:00:00 UTC, 1 січня 1970 року. Ця кількість секунд називається **Unix timestamp** і застосовується багатьма мовами програмування.

Крім методу `cmp`, в `SystemTime` є ще кілька корисних методів:

- `duration_since()`: повертає різницю в часі між двома датами у вигляді об'єкту `Duration`.
- `elapsed()`: повертає час, що минув з моменту, коли дата була встановлена, у вигляді об'єкту `Duration`.
- `now()`: повертає поточну дату та час.

Варто також зазначити, що дати в Rust є незмінними та не можуть бути модифікованими.

## Дивіться також

- [Розділ про `SystemTime` у документації Rust](https://doc.rust-lang.org/std/time/struct.SystemTime.html)
- [Приклади коду для роботи з датами в Rust](https://github.com/rust-lang/rust-by-example/blob/master/datetime/README.md)