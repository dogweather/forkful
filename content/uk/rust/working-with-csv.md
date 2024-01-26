---
title:                "Робота з CSV файлами"
html_title:           "Arduino: Робота з CSV файлами"
simple_title:         "Робота з CSV файлами"
programming_language: "Rust"
category:             "Rust"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/uk/rust/working-with-csv.md"
---

{{< edit_this_page >}}

## Що це та навіщо?
Робота з CSV — маніпуляція даними у форматі, де значення розділені комами. Програмісти використовують CSV через простоту обміну даними та широку підтримку у системах і програмах.

## Як це зробити:
Для роботи з CSV у Rust використовуємо бібліотеку `csv`. Встановлюємо її, додаючи `csv = "1.1.6"` до `Cargo.toml`.

```Rust
use csv;
use std::error::Error;
use std::io;
use std::process;

fn run() -> Result<(), Box<dyn Error>> {
    // Читаємо CSV
    let mut rdr = csv::Reader::from_reader(io::stdin());
    for result in rdr.records() {
        let record = result?;
        println!("{:?}", record);
    }
    Ok(())
}

fn main() {
    if let Err(err) = run() {
        println!("error running example: {}", err);
        process::exit(1);
    }
}
```
Виконання коду з прикладом CSV-даних виведе їх в консолі.

## Поглиблений огляд
CSV (Comma-Separated Values) — формат зберігання даних, поширений від 1970-х. Альтернативами є JSON, XML, YAML. Важливі деталі роботи з CSV в Rust:

- Ручне управління читанням/записом через структури `Reader`/`Writer`;
- Серіалізація/десеріалізація з/у спеціальні структури за допомогою `serde`.

## Дивись також
- Офіційна документація `csv` бібліотеки: [docs.rs/csv](https://docs.rs/csv)
- Серіалізація/десеріалізація з `serde`: [serde.rs](https://serde.rs)
- Робота з CSV в інших мовах програмування для порівняння: [Wikipedia](https://en.wikipedia.org/wiki/Comma-separated_values)
