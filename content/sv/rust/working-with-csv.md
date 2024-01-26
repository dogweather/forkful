---
title:                "Arbeta med csv"
html_title:           "Arduino: Arbeta med csv"
simple_title:         "Arbeta med csv"
programming_language: "Rust"
category:             "Rust"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/sv/rust/working-with-csv.md"
---

{{< edit_this_page >}}

## Vad & Varför?
CSV, eller "Comma-Separated Values", är ett enkelt filformat som används för att lagra tabulär data. Programmerare använder CSV för att enkelt utbyta data mellan olika system och program eftersom det är textbaserat och programoberoende.

## How to:
Rust har flera bibliotek för att hantera CSV-filer. Ett populärt val är `csv`-krate. Först, lägg till `csv = "1.1"` i din `Cargo.toml`. Här är hur du läser och skriver CSV:

```Rust
use std::error::Error;
use std::fs::File;
use std::io::{self, Write};
use csv::ReaderBuilder;
use csv::WriterBuilder;

fn read_csv(file_path: &str) -> Result<(), Box<dyn Error>> {
    let mut rdr = ReaderBuilder::new().from_path(file_path)?;
    for result in rdr.records() {
        let record = result?;
        println!("{:?}", record);
    }
    Ok(())
}

fn write_csv(file_path: &str, records: Vec<Vec<String>>) -> Result<(), Box<dyn Error>> {
    let file = File::create(file_path)?;
    let mut wtr = WriterBuilder::new().from_writer(io::BufWriter::new(file));
    for record in records {
        wtr.write_record(&record)?;
    }
    wtr.flush()?;
    Ok(())
}

fn main() -> Result<(), Box<dyn Error>> {
    read_csv("data.csv")?;
    write_csv("output.csv", vec![vec!["City".to_string(), "Population".to_string()], vec!["Stockholm".to_string(), "975551".to_string()]])?;
    Ok(())
}
```

Exempel på output vid läsning av CSV:

```
StringRecord(["Stockholm", "975551"])
```

## Deep Dive
CSV har sitt ursprung i tidiga datorsystem och blev en de facto-standard i början av 1970-talet. Trots nya format som JSON och XML är CSV fortfarande populärt för sin enkelhet och läsbarhet. När det gäller alternativ, hjälper bibliotek som `serde_csv` i Rust att hantera komplexa CSV-datamappningar med Rusts type system. För implementation, kan snabbhet och minneseffektivitet vara kritiska, och bibliotek som `csv` erbjuda möjligheter att läsa och skriva asynkront eller strömma stora filer.

## See Also
- [csv crate documentation](https://docs.rs/csv/latest/csv/)
- [CSV på Wikipedia](https://sv.wikipedia.org/wiki/CSV)
- [Serde: Serialization framework för Rust](https://serde.rs/)
- [Rust by Example: CSV-parsing](https://doc.rust-lang.org/stable/rust-by-example/std_misc/file/read_lines.html)
