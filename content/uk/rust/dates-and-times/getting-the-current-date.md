---
changelog:
- 2024-02-03, gpt-4-0125-preview, translated from English
date: 2024-02-03 19:11:19.130861-07:00
description: "\u041E\u0442\u0440\u0438\u043C\u0430\u043D\u043D\u044F \u043F\u043E\u0442\
  \u043E\u0447\u043D\u043E\u0457 \u0434\u0430\u0442\u0438 \u0443 Rust \u0454 \u043F\
  \u043E\u0448\u0438\u0440\u0435\u043D\u0438\u043C \u0437\u0430\u0432\u0434\u0430\u043D\
  \u043D\u044F\u043C \u0434\u043B\u044F \u0442\u0430\u043A\u0438\u0445 \u0437\u0430\
  \u0434\u0430\u0447, \u044F\u043A \u043B\u043E\u0433\u0443\u0432\u0430\u043D\u043D\
  \u044F, \u043E\u043F\u0435\u0440\u0430\u0446\u0456\u0457, \u0437\u0430\u0441\u043D\
  \u043E\u0432\u0430\u043D\u0456 \u043D\u0430 \u0447\u0430\u0441\u0456, \u0430\u0431\
  \u043E \u043F\u0440\u043E\u0441\u0442\u043E \u0432\u0456\u0434\u043E\u0431\u0440\
  \u0430\u0436\u0435\u043D\u043D\u044F \u0434\u0430\u0442\u0438. \u041D\u0430 \u0432\
  \u0456\u0434\u043C\u0456\u043D\u0443\u2026"
lastmod: '2024-03-13T22:44:48.954380-06:00'
model: gpt-4-0125-preview
summary: "\u041E\u0442\u0440\u0438\u043C\u0430\u043D\u043D\u044F \u043F\u043E\u0442\
  \u043E\u0447\u043D\u043E\u0457 \u0434\u0430\u0442\u0438 \u0443 Rust \u0454 \u043F\
  \u043E\u0448\u0438\u0440\u0435\u043D\u0438\u043C \u0437\u0430\u0432\u0434\u0430\u043D\
  \u043D\u044F\u043C \u0434\u043B\u044F \u0442\u0430\u043A\u0438\u0445 \u0437\u0430\
  \u0434\u0430\u0447, \u044F\u043A \u043B\u043E\u0433\u0443\u0432\u0430\u043D\u043D\
  \u044F, \u043E\u043F\u0435\u0440\u0430\u0446\u0456\u0457, \u0437\u0430\u0441\u043D\
  \u043E\u0432\u0430\u043D\u0456 \u043D\u0430 \u0447\u0430\u0441\u0456, \u0430\u0431\
  \u043E \u043F\u0440\u043E\u0441\u0442\u043E \u0432\u0456\u0434\u043E\u0431\u0440\
  \u0430\u0436\u0435\u043D\u043D\u044F \u0434\u0430\u0442\u0438. \u041D\u0430 \u0432\
  \u0456\u0434\u043C\u0456\u043D\u0443\u2026"
title: "\u041E\u0442\u0440\u0438\u043C\u0430\u043D\u043D\u044F \u043F\u043E\u0442\u043E\
  \u0447\u043D\u043E\u0457 \u0434\u0430\u0442\u0438"
---

{{< edit_this_page >}}

## Що і Чому?

Отримання поточної дати у Rust є поширеним завданням для таких задач, як логування, операції, засновані на часі, або просто відображення дати. На відміну від деяких мов, які включають функціонал роботи з датою та часом у свою стандартну бібліотеку, Rust заохочує використання потужної сторонньої бібліотеки, chrono, для всебічної маніпуляції з датою та часом через її переваги у функціональності та легкості використання.

## Як зробити:

### Використання стандартної бібліотеки Rust
Стандартна бібліотека Rust пропонує обмежений але швидкий спосіб отримання поточного часу, хоча й не безпосередньо поточної дати в календарному форматі. Ось як це робиться:

```rust
use std::time::{SystemTime, UNIX_EPOCH};

fn main() {
    match SystemTime::now().duration_since(UNIX_EPOCH) {
        Ok(n) => println!("Поточний час: {} секунд з моменту Unix Epoch.", n.as_secs()),
        Err(_) => panic!("SystemTime до Unix Epoch!"),
    }
}
```

Вивід:
```
Поточний час: 1615390665 секунд з моменту Unix Epoch.
```

### Використання бібліотеки Chrono
Для більш всебічного функціоналу роботи з датою та часом, включаючи отримання поточної дати, вам слід використовувати бібліотеку `chrono`. Спочатку додайте `chrono` до вашого `Cargo.toml`:

```toml
[dependencies]
chrono = "0.4"
```

Потім ви можете використовувати `chrono` для отримання поточної дати:

```rust
extern crate chrono;
use chrono::{Local, Datelike};

fn main() {
    let now = Local::now();
    println!("Поточна дата: {}-{}-{}", now.year(), now.month(), now.day());
}
```

Вивід:
```
Поточна дата: 2023-4-20
```

Бібліотека `chrono` робить роботу з датами та часом простою, пропонуючи широкий діапазон функціоналу, що виходить далеко за рамки простого отримання поточної дати, включаючи парсинг, форматування, та арифметичні операції над датами та часом.
