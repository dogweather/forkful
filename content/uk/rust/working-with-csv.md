---
title:                "Робота з CSV"
aliases:
- uk/rust/working-with-csv.md
date:                  2024-02-03T19:21:43.376142-07:00
model:                 gpt-4-0125-preview
simple_title:         "Робота з CSV"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/uk/rust/working-with-csv.md"
changelog:
  - 2024-02-03, gpt-4-0125-preview, translated from English
---

{{< edit_this_page >}}

## Що і чому?
Робота з файлами CSV (значення, розділені комами) полягає в читанні з та запису у прості текстові файли, які зберігають табличні дані. Програмісти роблять це, щоб уможливити обмін даними між різними програмами, системами або для ефективної обробки великих наборів даних у зручному для людини форматі.

## Як це зробити:
Rust, з його акцентом на безпеку та продуктивність, пропонує відмінні крейти (бібліотеки) для роботи з файлами CSV, серед яких найпопулярніший - `csv`. Вам також знадобиться `serde` для серіалізації та десеріалізації даних.

Спочатку додайте залежності до вашого `Cargo.toml`:

```toml
[dependencies]
csv = "1.1"
serde = { version = "1.0", features = ["derive"] }
```

### Читання CSV

Щоб прочитати файл CSV, визначте структуру, яка представляє ваші дані, і похідніть `Deserialize` від `serde`:

```rust
use serde::Deserialize;
use std::error::Error;
use std::fs::File;
use std::io;
use std::process;

#[derive(Debug, Deserialize)]
struct Record {
    city: String,
    state: String,
    population: u64,
}

fn read_from_csv(file_path: &str) -> Result<(), Box<dyn Error>> {
    let file = File::open(file_path)?;
    let mut rdr = csv::Reader::from_reader(file);

    for result in rdr.deserialize() {
        let record: Record = result?;
        println!("{:?}", record);
    }
    Ok(())
}

fn main() {
    if let Err(err) = read_from_csv("cities.csv") {
        println!("помилка при виконанні прикладу: {}", err);
        process::exit(1);
    }
}
```

Приклад виводу для CSV з інформацією про міста може виглядати так:
```plaintext
Record { city: "Seattle", state: "WA", population: 744955 }
Record { city: "New York", state: "NY", population: 8336817 }
```

### Запис у CSV

Щоб записати в файл CSV, визначте структуру і похідніть `Serialize`:

```rust
use serde::Serialize;
use std::error::Error;
use std::fs::File;

#[derive(Serialize)]
struct Record {
    city: String,
    state: String,
    population: u64,
}

fn write_to_csv(file_path: &str, records: Vec<Record>) -> Result<(), Box<dyn Error>> {
    let file = File::create(file_path)?;
    let mut wtr = csv::Writer::from_writer(file);

    for record in records {
        wtr.serialize(&record)?;
    }
    wtr.flush()?;
    Ok(())
}

fn main() -> Result<(), Box<dyn Error>> {
    let records = vec![
        Record {
            city: "Los Angeles".into(),
            state: "CA".into(),
            population: 3979563,
        },
        Record {
            city: "Chicago".into(),
            state: "IL".into(),
            population: 2695598,
        },
    ];

    write_to_csv("output.csv", records)?;

    Ok(())
}
```

Це створить `output.csv` з даними:

```csv
city,state,population
Los Angeles,CA,3979563
Chicago,IL,2695598
```

Використовуючи потужну систему типів Rust та надійні крейти екосистеми, робота з даними CSV стає одночасно ефективною та простою, забезпечуючи безпеку та продуктивність ваших завдань з обробки даних.
