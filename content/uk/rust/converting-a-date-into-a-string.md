---
title:                "Перетворення дати в рядок"
html_title:           "Rust: Перетворення дати в рядок"
simple_title:         "Перетворення дати в рядок"
programming_language: "Rust"
category:             "Rust"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/uk/rust/converting-a-date-into-a-string.md"
---

{{< edit_this_page >}}

## Чому

Конвертація дати в рядок є важливою задачею в багатьох програмах, особливо веб-додатках, де необхідно відображати дати у зручному для користувача форматі. Також, виконання цієї задачі допоможе вам покращити свої навички програмування на мові Rust.

## Як

Існує багато різних методів конвертації дати в рядок у мові Rust. Використовуючи бібліотеку `chrono`, ми можемо надати коректний формат для дати та часу:

```Rust
use chrono::{DateTime, Utc, Local, Datelike, Timelike, TimeZone, FixedOffset};

fn main() {
    // Демонстраційна дата
    let date = Utc.ymd(2021, 9, 8).and_hms(12, 0, 0);
    println!("{}", date.to_rfc2822());
    println!("{}", date.to_string());
    println!("{}", date.format("%Y-%m-%d %H:%M:%S").to_string());
}
```

Вивід:

```
Wed, 08 Sep 2021 12:00:00 +0000
2021-09-08 12:00:00 UTC
2021-09-08 12:00:00
```

Варто зазначити, що `to_rfc2822()` та `to_string()` використовують різні формати дати та часу. Також, можна використати метод `format()` з подальшим заданням бажаного формату дати та часу за допомогою спеціальних патернів.

## Deep Dive

У більш складних програмах, може виникнути необхідність використовувати іншу часову зону або виводити дату у місцевому часі. Для цього, можна використовувати структуру `FixedOffset`. Крім того, більш детально про функції та можливості бібліотеки `chrono` можна дізнатися у її [офіційній документації](https://docs.rs/chrono/latest/chrono/).

## Дивись Також

- [https://doc.rust-lang.org/std/time/struct.SystemTime.html](https://doc.rust-lang.org/std/time/struct.SystemTime.html)
- [https://www.lpalmieri.com/posts/2020-10-11-datetime-rust/](https://www.lpalmieri.com/posts/2020-10-11-datetime-rust/)
- [https://www.twilio.com/blog/converting-datetime-rust](https://www.twilio.com/blog/converting-datetime-rust)