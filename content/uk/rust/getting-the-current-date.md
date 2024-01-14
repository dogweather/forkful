---
title:                "Rust: Отримання поточної дати."
simple_title:         "Отримання поточної дати."
programming_language: "Rust"
category:             "Rust"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/uk/rust/getting-the-current-date.md"
---

{{< edit_this_page >}}

## Чому

Отримання поточної дати є важливою складовою для багатьох програм, особливо тих, що використовуються для обробки даних та створення звітів. У Rust є простий та ефективний спосіб отримати поточну дату, що допоможе зберегти час та зусилля програмістів.

## Як це зробити

В Rust існує стандартна бібліотека з функціями для роботи з часом. Щоб отримати поточну дату, спочатку потрібно імпортувати бібліотеки `std::time::SystemTime` та `std::time::SystemTimeError`. Тоді можна викликати функцію `now()` з `SystemTime`, що поверне поточний час у форматі `SystemTime`.

Наприклад, якщо ми хочемо отримати дату у форматі `%Y-%m-%d`, можна використати метод `format()` з бібліотеки `chrono`, яка надає інструменти для роботи зі строковими даними дати та часу. Далі, результат необхідно сконвертувати у строку та вивести на екран.

```
Rust
use std::time::{SystemTime, SystemTimeError};
use chrono::prelude::*;

fn main() {
    let current_date = SystemTime::now();
    let formatted_date = current_date
        .duration_since(SystemTime::UNIX_EPOCH)
        .map(|d| Utc.timestamp(d.as_secs() as i64, 0).format("%Y-%m-%d").to_string())
        .unwrap_or_else(|e| e.to_string());

    println!("Potочна дата: {}", formatted_date);
}
```

Вивід на екран буде виглядати так:

```
Potочна дата: 2019-11-24
```

## Глибокая інформація

Якщо більше зацікавлених у наступну побітову глибоку розбірку цього процесу, можна дізнатися більше про `SystemTime` та `SystemTimeError` у [документації Rust](https://doc.rust-lang.org/std/time/struct.SystemTime.html). Також, для роботи з форматуванням дати і часу, можна перевірити більше про [chrono бібліотеку](https://docs.rs/chrono/0.4.10/chrono/).

## Дивись також

- [Робота зі строковими даними дати та часу в Rust](https://www.phoronix.com/scan.php?page=news_item&px=Rust-Date-Time-Formats)
- [Створення графіку дати з даними в Rust](https://rust-lang-nursery.github.io/date-time/guide-conversion.html)
- [Порівняння дат у Rust](https://raymii.org/s/blog/Comparing_dates_with_Rust__the_easy_way.html)