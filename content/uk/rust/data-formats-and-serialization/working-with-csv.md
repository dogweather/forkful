---
changelog:
- 2024-02-03, gpt-4-0125-preview, translated from English
date: 2024-02-03 19:21:43.376142-07:00
description: "\u0420\u043E\u0431\u043E\u0442\u0430 \u0437 \u0444\u0430\u0439\u043B\
  \u0430\u043C\u0438 CSV (\u0437\u043D\u0430\u0447\u0435\u043D\u043D\u044F, \u0440\
  \u043E\u0437\u0434\u0456\u043B\u0435\u043D\u0456 \u043A\u043E\u043C\u0430\u043C\u0438\
  ) \u043F\u043E\u043B\u044F\u0433\u0430\u0454 \u0432 \u0447\u0438\u0442\u0430\u043D\
  \u043D\u0456 \u0437 \u0442\u0430 \u0437\u0430\u043F\u0438\u0441\u0443 \u0443 \u043F\
  \u0440\u043E\u0441\u0442\u0456 \u0442\u0435\u043A\u0441\u0442\u043E\u0432\u0456\
  \ \u0444\u0430\u0439\u043B\u0438, \u044F\u043A\u0456 \u0437\u0431\u0435\u0440\u0456\
  \u0433\u0430\u044E\u0442\u044C \u0442\u0430\u0431\u043B\u0438\u0447\u043D\u0456\
  \ \u0434\u0430\u043D\u0456. \u041F\u0440\u043E\u0433\u0440\u0430\u043C\u0456\u0441\
  \u0442\u0438 \u0440\u043E\u0431\u043B\u044F\u0442\u044C\u2026"
lastmod: 2024-02-19 22:05:07.987732
model: gpt-4-0125-preview
summary: "\u0420\u043E\u0431\u043E\u0442\u0430 \u0437 \u0444\u0430\u0439\u043B\u0430\
  \u043C\u0438 CSV (\u0437\u043D\u0430\u0447\u0435\u043D\u043D\u044F, \u0440\u043E\
  \u0437\u0434\u0456\u043B\u0435\u043D\u0456 \u043A\u043E\u043C\u0430\u043C\u0438\
  ) \u043F\u043E\u043B\u044F\u0433\u0430\u0454 \u0432 \u0447\u0438\u0442\u0430\u043D\
  \u043D\u0456 \u0437 \u0442\u0430 \u0437\u0430\u043F\u0438\u0441\u0443 \u0443 \u043F\
  \u0440\u043E\u0441\u0442\u0456 \u0442\u0435\u043A\u0441\u0442\u043E\u0432\u0456\
  \ \u0444\u0430\u0439\u043B\u0438, \u044F\u043A\u0456 \u0437\u0431\u0435\u0440\u0456\
  \u0433\u0430\u044E\u0442\u044C \u0442\u0430\u0431\u043B\u0438\u0447\u043D\u0456\
  \ \u0434\u0430\u043D\u0456. \u041F\u0440\u043E\u0433\u0440\u0430\u043C\u0456\u0441\
  \u0442\u0438 \u0440\u043E\u0431\u043B\u044F\u0442\u044C\u2026"
title: "\u0420\u043E\u0431\u043E\u0442\u0430 \u0437 CSV"
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
