---
title:                "Отримання поточної дати"
html_title:           "Rust: Отримання поточної дати"
simple_title:         "Отримання поточної дати"
programming_language: "Rust"
category:             "Rust"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/uk/rust/getting-the-current-date.md"
---

{{< edit_this_page >}}

## Чому

Встановлення поточної дати в програмі може бути корисним для відстеження часу створення або модифікації файлів, роботи з розкладами або для створення інформативних повідомлень.

## Як

```Rust
use std::time::SystemTime;

fn main() {
    // Отримання поточного часу у секундах з 1 січня 1970 року (Unix Epoch)
    let current_time = SystemTime::now().duration_since(SystemTime::UNIX_EPOCH).expect("Could not get current time");

    // Перетворення у формат UNIX Timestamp (із секунд у мікросекунди)
    let unix_timestamp = current_time.as_secs() as u64;

    println!("Поточний час у UNIX Timestamp: {}", unix_timestamp);
}


```

Вивід:
``` 
Поточний час у UNIX Timestamp: 1615038182 
```

## Глибока Занурення

В Rust, є кілька способів отримати поточну дату та час, уключаючи використання вбудованих типів даних `SystemTime` та `Instant`, а також стандартної бібліотеки `std::time`. Ці типи даних дозволяють отримувати поточну дату та час у різних форматах, включаючи UNIX Timestamp та `DateTime` об'єкти.

Щоб отримати більше інформації про отримання поточної дати в Rust, ознайомтеся з офіційною документацією [тут](https://doc.rust-lang.org/std/time/index.html).

## Дивись також

- [Official Rust Documentation - Time](https://doc.rust-lang.org/std/time/index.html)
- [A Guide to Working With Dates and Times in Rust](https://www.freecodecamp.org/news/how-to-work-with-dates-and-times-in-rust/)