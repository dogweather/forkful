---
title:                "Rust: Отримання поточної дати."
programming_language: "Rust"
category:             "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/uk/rust/getting-the-current-date.md"
---

{{< edit_this_page >}}

## Чому

Одним із найпоширеніших завдань у програмуванні є отримання поточної дати. Це важливо для багатьох застосувань, таких як облік часу, генерація звітності та поділка даних за датами. У цьому блозі ми розглянемо, як отримати поточну дату за допомогою мови програмування Rust.

## Як отримати поточну дату у Rust

Для того, щоб отримати поточну дату у Rust, найпростіший спосіб - використати стандартний модуль `std::time::SystemTime`. Почніть зі створення змінної, яка буде зберігати поточний час:

```Rust
let current_time = std::time::SystemTime::now();
```

Для отримання зрозумілого для людини представлення часу, ми можемо використати функцію `format()`:

```Rust
let date = std::time::SystemTime::now();
let human_readable = date.format("%a, %d %b %Y %T");
println!("Current date and time: {}", human_readable); 
```

У цьому прикладі ми використали формат дати для отримання дня тижня, дня місяця, місяця, року та часу. Щоб дізнатися додаткову інформацію про формати дати, ви можете переглянути [цю документацію.](https://doc.rust-lang.org/std/time/index.html#strftime-%28огляд-%29)

## Занурення у отримання поточної дати

Пригадайте, що стандартний модуль `std::time::SystemTime` повертає час у вигляді кількості секунд, які пройшли з 1 січня 1970 року. Це називається [Unix-часом.](https://uk.wikipedia.org/wiki/Unix-час) Це потрібно для того, щоб програма могла працювати незалежно від різних часових зон у світі.

Щоб отримати час у нормальному форматі, вам може знадобитися використати стандартний модуль `chrono`, який надає широкі можливості для роботи з датами і часом у Rust. Ми можемо використати його для створення об'єкта `DateTime` з дати, яку ми отримали від `SystemTime`:

```Rust
let current_time = std::time::SystemTime::now();
let date_time = chrono::DateTime::<chrono::offset::Utc>::from(current_time);
```

Також, співставляючи часові зони, можна використати метод `with_timezone()`:

```Rust
let local_date_time = date_time.with_timezone(&chrono::Local);
```

Тепер ми можемо використати функцію `format()` для представлення часу у зрозумілому форматі для нашої часової зони:

```Rust
let current_time = std::time::SystemTime::now();
let date_time = chrono::DateTime::<chrono::offset::Utc>::from(current