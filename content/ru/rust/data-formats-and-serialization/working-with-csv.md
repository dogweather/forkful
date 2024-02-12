---
title:                "Работа с CSV"
aliases:
- /ru/rust/working-with-csv.md
date:                  2024-01-29T00:04:19.748144-07:00
model:                 gpt-4-0125-preview
simple_title:         "Работа с CSV"

tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ru/rust/working-with-csv.md"
changelog:
  - 2024-01-29, gpt-4-0125-preview, translated from English
---

{{< edit_this_page >}}

## Что и зачем?

CSV, что означает значения, разделённые запятыми, - это формат файла, используемый для хранения табличных данных. Программисты обожают CSV за его простоту и широкую поддержку инструментами и языками программирования для манипуляции с данными, импорта и экспорта.

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
