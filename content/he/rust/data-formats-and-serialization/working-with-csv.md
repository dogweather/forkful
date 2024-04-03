---
changelog:
- 2024-02-03, gpt-4-0125-preview, translated from English
date: 2024-02-03 19:21:49.777741-07:00
description: "\u05E2\u05D1\u05D5\u05D3\u05D4 \u05E2\u05DD \u05E7\u05D1\u05E6\u05D9\
  \ CSV (\u05E2\u05E8\u05DB\u05D9\u05DD \u05DE\u05D5\u05E4\u05E8\u05D3\u05D9\u05DD\
  \ \u05D1\u05E4\u05E1\u05D9\u05E7) \u05E2\u05D5\u05E1\u05E7\u05EA \u05D1\u05E7\u05E8\
  \u05D9\u05D0\u05D4 \u05DE\u05EA\u05D5\u05DA \u05D5\u05DB\u05EA\u05D9\u05D1\u05D4\
  \ \u05D0\u05DC \u05E7\u05D5\u05D1\u05E6\u05D9 \u05D8\u05E7\u05E1\u05D8 \u05E4\u05E9\
  \u05D5\u05D8\u05D9\u05DD \u05D4\u05E9\u05D5\u05DE\u05E8\u05D9\u05DD \u05E0\u05EA\
  \u05D5\u05E0\u05D9\u05DD \u05D8\u05D1\u05DC\u05D0\u05D9\u05D9\u05DD. \u05DE\u05EA\
  \u05DB\u05E0\u05EA\u05D9\u05DD \u05E2\u05D5\u05E9\u05D9\u05DD \u05D6\u05D0\u05EA\
  \ \u05DB\u05D3\u05D9 \u05DC\u05D0\u05E4\u05E9\u05E8 \u05E9\u05D9\u05EA\u05D5\u05E3\
  \ \u05E0\u05EA\u05D5\u05E0\u05D9\u05DD \u05D1\u05D9\u05DF\u2026"
lastmod: '2024-03-13T22:44:39.024417-06:00'
model: gpt-4-0125-preview
summary: "\u05E2\u05D1\u05D5\u05D3\u05D4 \u05E2\u05DD \u05E7\u05D1\u05E6\u05D9 CSV\
  \ (\u05E2\u05E8\u05DB\u05D9\u05DD \u05DE\u05D5\u05E4\u05E8\u05D3\u05D9\u05DD \u05D1\
  \u05E4\u05E1\u05D9\u05E7) \u05E2\u05D5\u05E1\u05E7\u05EA \u05D1\u05E7\u05E8\u05D9\
  \u05D0\u05D4 \u05DE\u05EA\u05D5\u05DA \u05D5\u05DB\u05EA\u05D9\u05D1\u05D4 \u05D0\
  \u05DC \u05E7\u05D5\u05D1\u05E6\u05D9 \u05D8\u05E7\u05E1\u05D8 \u05E4\u05E9\u05D5\
  \u05D8\u05D9\u05DD \u05D4\u05E9\u05D5\u05DE\u05E8\u05D9\u05DD \u05E0\u05EA\u05D5\
  \u05E0\u05D9\u05DD \u05D8\u05D1\u05DC\u05D0\u05D9\u05D9\u05DD."
title: "\u05E2\u05D5\u05D1\u05D3\u05D9\u05DD \u05E2\u05DD CSV"
weight: 37
---

## איך לעשות:
ראסט, עם התמקדותו בביטחון וביצועים, מציע ספריות (crates) מעולות לעבודה עם קבצי CSV, כאשר `csv` היא הפופולרית ביותר. תצטרך גם את `serde` לשריאליזציה ודה-שריאליזציה של נתונים.

ראשית, הוסף את התלותיות ל-`Cargo.toml` שלך:

```toml
[dependencies]
csv = "1.1"
serde = { version = "1.0", features = ["derive"] }
```

### קריאת CSV
כדי לקרוא קובץ CSV, הגדר מבנה שמייצג את הנתונים שלך ונגזר `Deserialize` מ-`serde`:

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
        println!("error running example: {}", err);
        process::exit(1);
    }
}
```

דוגמת פלט עבור CSV עם מידע על ערים עשויה להיראות כך:
```plaintext
Record { city: "Seattle", state: "WA", population: 744955 }
Record { city: "New York", state: "NY", population: 8336817 }
```

### כתיבה ל-CSV
כדי לכתוב לקובץ CSV, הגדר מבנה ונגזר `Serialize`:

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

זה ייצור את `output.csv` עם הנתונים:

```csv
city,state,population
Los Angeles,CA,3979563
Chicago,IL,2695598
```

בזכות מערכת הטיפוסים החזקה של ראסט וחבילות ה-ecosystem האמינות, העבודה עם נתוני CSV הופכת ליעילה ופשוטה, מבטיחה ביטחון וביצועים במשימות עיבוד הנתונים שלך.
