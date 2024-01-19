---
title:                "Аналіз дати з рядка"
html_title:           "C++: Аналіз дати з рядка"
simple_title:         "Аналіз дати з рядка"
programming_language: "Rust"
category:             "Rust"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/uk/rust/parsing-a-date-from-a-string.md"
---

{{< edit_this_page >}}

## Що і чому?
**Розбір дати з рядка** - це процес вилучення конкретного значення дати з текстового рядка. Програмісти роблять це, коли потрібно працювати з датою, яка була отримана у форматі рядка, наприклад, при отриманні даних з API.

## Як це зробити:
Нижче наведений приклад коду на Rust який працює з бібліотекою `chrono` для розбору дати з рядка.

```Rust
use chrono::{DateTime, NaiveDate, NaiveDateTime, NaiveTime, TimeZone, Utc};

let date_str = "2022-09-03";
let parse_from_str = NaiveDate::parse_from_str(date_str, "%Y-%m-%d");
match parse_from_str {
    Ok(date) => println!("Відповідь: {:?}", date),
    Err(e) => println!("Помилка: {:?}", e),
}
```
Приклад вище виведе: 
```
Відповідь: NaiveDate(2022, 9, 3)
```

## Поглиблений занурення
1. **Історичний контекст**: Розбір дати з рядка був важливим аспектом програмування майже з самого початку історії комп'ютерних наук. З розширенням глобальних мереж, особливо з розвитком Інтернету, ця потреба стала більш важливою.
2. **Альтернативи**: Rust пропонує ряд альтернатив для розбору дати, в тому числі: бібліотеки `time`, `date` і `rust-datetime`.
3. **Деталі реалізації**: У Rust, дата представлена як структура `NaiveDate` і має метод `parse_from_str`, якому передається рядок і формат дати. Цей метод повертає `Result`, що розпаковується при помилці.

## Додатково
Докладніше про розбір дати с рядка в Руст можна дізнатися з наступних ресурсів:
- Документація `chrono` - [chrono.rs](https://docs.rs/chrono/0.4.19/chrono/)
- Практичний приклад на `stackoverflow` - [stackoverflow.com](https://stackoverflow.com/questions/41464719/how-to-parse-a-date-string-in-rust)
- Документація `NaiveDate` - [chrono::naive::date](https://docs.rs/chrono/0.4.2/chrono/naive/date/struct.NaiveDate.html)