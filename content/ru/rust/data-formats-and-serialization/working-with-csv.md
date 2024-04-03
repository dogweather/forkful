---
changelog:
- 2024-01-29, gpt-4-0125-preview, translated from English
date: 2024-01-29 00:04:19.748144-07:00
description: "\u041A\u0430\u043A \u044D\u0442\u043E \u0441\u0434\u0435\u043B\u0430\
  \u0442\u044C: \u0421\u043D\u0430\u0447\u0430\u043B\u0430 \u0434\u043E\u0431\u0430\
  \u0432\u044C\u0442\u0435 \u043D\u0435\u043E\u0431\u0445\u043E\u0434\u0438\u043C\u044B\
  \u0439 \u043A\u043E\u043D\u0442\u0435\u0439\u043D\u0435\u0440 \u0432 `Cargo.toml`."
lastmod: '2024-03-13T22:44:44.706142-06:00'
model: gpt-4-0125-preview
summary: "\u0421\u043D\u0430\u0447\u0430\u043B\u0430 \u0434\u043E\u0431\u0430\u0432\
  \u044C\u0442\u0435 \u043D\u0435\u043E\u0431\u0445\u043E\u0434\u0438\u043C\u044B\u0439\
  \ \u043A\u043E\u043D\u0442\u0435\u0439\u043D\u0435\u0440 \u0432 `Cargo.toml`."
title: "\u0420\u0430\u0431\u043E\u0442\u0430 \u0441 CSV"
weight: 37
---

## Как это сделать:
Сначала добавьте необходимый контейнер в `Cargo.toml`:

```toml
[dependencies]
csv = "1.1"
```

Затем обработайте чтение CSV:

```rust
use csv::Reader;
use std::error::Error;

fn main() -> Result<(), Box<dyn Error>> {
    let mut rdr = Reader::from_path("data.csv")?;
    for result in rdr.records() {
        let record = result?;
        println!("{:?}", record);
    }
    Ok(())
}
```

Запись в CSV:

```rust
use csv::Writer;
use std::error::Error;

fn main() -> Result<(), Box<dyn Error>> {
    let mut wtr = Writer::from_path("output.csv")?;
    wtr.write_record(&["name", "city", "age"])?;
    wtr.write_record(&["Jane", "New York", "30"])?;
    wtr.flush()?;
    Ok(())
}
```

Пример вывода для чтения:

```
StringRecord(["Jane", "New York", "30"])
```

## Погружение в тему
CSV существует с первых дней персональных компьютеров, используется для обмена данными между программами и системами. Хотя JSON и XML предоставляют больше структурированности, CSV остаётся популярным за его легковесность и простоту использования.

Альтернативы контейнеру csv в Rust включают `serde_csv`, предлагающий удобную сериализацию и десериализацию, и `papercut`, сосредоточенный на безопасном и удобном разборе CSV.

Разбор CSV в Rust ограничен вводом-выводом. Эффективная обработка включает использование итераторов и надежную обработку ошибок в Rust для управления неправильно оформленными данными.

## См. также
- Документация по контейнеру CSV в Rust: https://docs.rs/csv/
- Книга "Язык программирования Rust": https://doc.rust-lang.org/book/
- Serde: https://serde.rs/ - фреймворк для сериализации и десериализации структур данных Rust.
- Rust на примере, CSV: https://rustbyexample.com/std_misc/file/csv.html
