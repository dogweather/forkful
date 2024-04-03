---
changelog:
- 2024-01-28, gpt-4-0125-preview, translated from English
date: 2024-01-28 23:55:20.912002-07:00
description: "\u041A\u0430\u043A \u044D\u0442\u043E \u0441\u0434\u0435\u043B\u0430\
  \u0442\u044C: \u0414\u043B\u044F \u0440\u0430\u0431\u043E\u0442\u044B \u0441 \u0434\
  \u0430\u0442\u0430\u043C\u0438 \u0432 Rust \u0438\u0441\u043F\u043E\u043B\u044C\u0437\
  \u0443\u0435\u0442\u0441\u044F `chrono`. \u0421\u043D\u0430\u0447\u0430\u043B\u0430\
  \ \u0432 `cargo.toml` \u043D\u0443\u0436\u043D\u043E \u0434\u043E\u0431\u0430\u0432\
  \u0438\u0442\u044C `chrono = \"0.4\"`. \u041F\u043E\u0441\u043B\u0435 \u044D\u0442\
  \u043E\u0433\u043E \u0432\u044B \u043C\u043E\u0436\u0435\u0442\u0435 \u0441\u0440\
  \u0430\u0432\u043D\u0438\u0442\u044C\u2026"
lastmod: '2024-03-13T22:44:44.688593-06:00'
model: gpt-4-0125-preview
summary: "\u0414\u043B\u044F \u0440\u0430\u0431\u043E\u0442\u044B \u0441 \u0434\u0430\
  \u0442\u0430\u043C\u0438 \u0432 Rust \u0438\u0441\u043F\u043E\u043B\u044C\u0437\u0443\
  \u0435\u0442\u0441\u044F `chrono`."
title: "\u0421\u0440\u0430\u0432\u043D\u0435\u043D\u0438\u0435 \u0434\u0432\u0443\u0445\
  \ \u0434\u0430\u0442"
weight: 27
---

## Как это сделать:
Для работы с датами в Rust используется `chrono`. Сначала в `cargo.toml` нужно добавить `chrono = "0.4"`. После этого вы можете сравнить даты следующим образом:

```Rust
extern crate chrono;
use chrono::{DateTime, Utc};

fn main() {
    let date1: DateTime<Utc> = Utc::now();
    let date2: DateTime<Utc> = Utc::now(); // Измените это для разных результатов

    if date1 > date2 {
        println!("Date1 позже Date2");
    } else if date1 < date2 {
        println!("Date1 раньше Date2");
    } else {
        println!("Date1 равна Date2");
    }
}
```

Пример вывода, где `date1` позже:

```
Date1 позже Date2
```

## Подробнее
В начальные дни Rust (2010-е годы) сравнение дат было сложнее — не было крейта `chrono`. Появление `chrono` упростило процесс за счёт таких типов, как `DateTime`. До `chrono` мы обрабатывали время вручную, что могло привести к ошибкам.

Почему `chrono`? Он абстрагирует сложности, такие как часовые пояса и високосные годы, делая сравнение дат надёжным. Без него пришлось бы работать с Unix-временными метками, что громоздко и менее читабельно.

Существуют альтернативы `chrono`, такие как крейт `time`, но `chrono` широко используется из-за его простоты и функциональности.

## См. также
- Документация к крейту `chrono`: [docs.rs/chrono](https://docs.rs/chrono/)
- Официальная документация Rust по концепции даты и времени: [doc.rust-lang.org/std/time](https://doc.rust-lang.org/std/time/index.html)
- Сравнение крейтов `chrono` и `time`: [users.rust-lang.org](https://users.rust-lang.org/t/chrono-vs-time/45575)
