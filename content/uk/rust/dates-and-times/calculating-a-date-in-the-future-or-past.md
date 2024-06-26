---
date: 2024-01-20 17:32:03.493758-07:00
description: "\u042F\u043A \u0446\u0435 \u0437\u0440\u043E\u0431\u0438\u0442\u0438\
  : \u0412 Rust \u0432\u0438\u043A\u043E\u0440\u0438\u0441\u0442\u043E\u0432\u0443\
  \u0454\u043C\u043E \u0431\u0456\u0431\u043B\u0456\u043E\u0442\u0435\u043A\u0443\
  \ `chrono` \u0434\u043B\u044F \u0440\u043E\u0431\u043E\u0442\u0438 \u0437 \u0434\
  \u0430\u0442\u0430\u043C\u0438. \u0429\u043E\u0431 \u0434\u043E\u0434\u0430\u0442\
  \u0438 \u0446\u044E \u0431\u0456\u0431\u043B\u0456\u043E\u0442\u0435\u043A\u0443\
  \ \u0434\u043E \u043F\u0440\u043E\u0435\u043A\u0442\u0443, \u0434\u043E\u0434\u0430\
  \u0454\u043C\u043E \u043D\u0430\u0441\u0442\u0443\u043F\u043D\u0435 \u0432 `Cargo.toml`."
lastmod: '2024-03-13T22:44:48.959773-06:00'
model: gpt-4-1106-preview
summary: "\u0412 Rust \u0432\u0438\u043A\u043E\u0440\u0438\u0441\u0442\u043E\u0432\
  \u0443\u0454\u043C\u043E \u0431\u0456\u0431\u043B\u0456\u043E\u0442\u0435\u043A\u0443\
  \ `chrono` \u0434\u043B\u044F \u0440\u043E\u0431\u043E\u0442\u0438 \u0437 \u0434\
  \u0430\u0442\u0430\u043C\u0438."
title: "\u041E\u0431\u0447\u0438\u0441\u043B\u0435\u043D\u043D\u044F \u0434\u0430\u0442\
  \u0438 \u0443 \u043C\u0430\u0439\u0431\u0443\u0442\u043D\u044C\u043E\u043C\u0443\
  \ \u0430\u0431\u043E \u043C\u0438\u043D\u0443\u043B\u043E\u043C\u0443"
weight: 26
---

## Як це зробити:
В Rust використовуємо бібліотеку `chrono` для роботи з датами. Щоб додати цю бібліотеку до проекту, додаємо наступне в `Cargo.toml`:

```toml
[dependencies]
chrono = "0.4"
```

Тепер можна обчислити дату у майбутньому чи минулому:

```rust
extern crate chrono;
use chrono::{DateTime, Duration, Utc};

fn main() {
    let now = Utc::now();
    let two_weeks = Duration::weeks(2);

    let future_date = now + two_weeks;
    let past_date = now - two_weeks;

    println!("Теперішня дата: {}", now);
    println!("Дата через два тижні: {}", future_date);
    println!("Дата два тижні тому: {}", past_date);
}
```

У вас буде щось на кшталт цього:

```
Теперішня дата: 2023-04-07T12:34:56Z
Дата через два тижні: 2023-04-21T12:34:56Z
Дата два тижні тому: 2023-03-24T12:34:56Z
```

## Глибше занурення:
Обчислення дат має давню історію - люди рахували дні для аграрних циклів, свят і подій. У цифрову епоху, це стало частиною систем управління проектами, планувальників і засобів для відстеження часу.

Альтернативи `chrono` включають безпосередню роботу з таймстемпами і стандартну бібліотеку (`std::time`). Але `chrono` надає більш зручні інструменти для роботи з датами.

Деталі реалізації: `chrono` базується на стандартних часових точках, але також дозволяє форматування дат, обчислення різниці між датами і створення настроюваних часових зон. Враховуйте, що обчислення дат важливе для багатьох функцій і може впливати на продуктивність системи.

## Дивіться також:
- Документація `chrono`: https://docs.rs/chrono/
- Стандартна бібліотека часу Rust: https://doc.rust-lang.org/std/time/
- Практичний посібник з Rust і дат/часу: https://stevedonovan.github.io/rustifications/2018/05/31/rust-datetime-manipulation.html
