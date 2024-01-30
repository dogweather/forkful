---
title:                "Преобразование даты в строку"
date:                  2024-01-28T23:56:41.382313-07:00
model:                 gpt-4-0125-preview
simple_title:         "Преобразование даты в строку"
programming_language: "Rust"
category:             "Rust"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ru/rust/converting-a-date-into-a-string.md"
changelog:
  - 2024-01-28, gpt-4-0125-preview, translated from English
---

{{< edit_this_page >}}

## Что и Зачем?

Преобразование даты в строку в Rust позволяет нам отображать даты в формате, удобном для восприятия людьми. Мы делаем это для пользовательских интерфейсов, логов или любых мест, где людям нужно понимать даты.

## Как это сделать:

Для работы с датой и временем в Rust используется крейт `chrono`. Убедитесь, что он есть в вашем `Cargo.toml`:

```toml
[dependencies]
chrono = "0.4"
```

Теперь давайте отформатируем дату как строку.

```rust
extern crate chrono;
use chrono::{DateTime, Utc, NaiveDateTime};

fn main() {
    let date: DateTime<Utc> = Utc::now(); // Получаем текущую дату и время в UTC.
    let formatted_date = date.format("%Y-%m-%d %H:%M:%S").to_string();
    println!("{}", formatted_date); // Выводит: 2023-03-15 14:30:45
}
```

## Подробнее

До появления `chrono` в стандартной библиотеке Rust было несколько функций для работы с датой и временем, но они были базовыми. `chrono` построен на этом фундаменте и предлагает комплексный функционал. Альтернативой может быть новый крейт `time` от Rust, который стремится к более безопасному и удобному API.

Когда вы преобразуете дату в строку, вы сериализуете – превращаете данные в формат, который можно передавать или хранить. Выбранный вами формат (`%Y-%m-%d %H:%M:%S` в нашем случае) на ваше усмотрение, и `chrono` поддерживает множество таких шаблонов.

Внутренне, даты часто хранятся в виде временных отметок – секунд с начальной точки, например, с эпохи Unix (1 января 1970 года). Когда вы форматируете дату, вы вычисляете читаемую человеком форму из этого счета, учитывая часовые пояса и високосные секунды.

## См. также

- Документация крейта `chrono`: https://docs.rs/chrono/
- Документация крейта `time` от Rust: https://docs.rs/time/
- Синтаксис форматирования дат: http://www.unicode.org/reports/tr35/tr35-dates.html#Date_Field_Symbol_Table