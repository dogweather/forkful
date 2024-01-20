---
title:                "Arbeiten mit CSV-Dateien"
html_title:           "Arduino: Arbeiten mit CSV-Dateien"
simple_title:         "Arbeiten mit CSV-Dateien"
programming_language: "Rust"
category:             "Rust"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/de/rust/working-with-csv.md"
---

{{< edit_this_page >}}

## Was & Warum?
CSV (Comma-Separated Values) ist ein einfaches Format für tabellarische Daten. Programmierer nutzen es gerne wegen seiner Simplizität und weitgehenden Unterstützung in Werkzeugen und Programmiersprachen.

## How to:
Installiere das `csv` Crate und füge es in `Cargo.toml` hinzu:

```toml
[dependencies]
csv = "1.1"
serde = { version = "1.0", features = ["derive"] }
```

Ein einfaches Beispiel zum Lesen einer CSV-Datei:

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

Beim Schreiben in eine CSV-Datei:

```rust
use csv::Writer;
use std::error::Error;

fn main() -> Result<(), Box<dyn Error>> {
    let mut wtr = Writer::from_path("output.csv")?;
    
    wtr.write_record(&["Stadt", "Land", "Bevölkerung"])?;
    wtr.write_record(&["Berlin", "Deutschland", "3,645,000"])?;
    wtr.write_record(&["Hamburg", "Deutschland", "1,841,000"])?;
    
    wtr.flush()?;
    
    Ok(())
}
```

## Deep Dive
CSV ist seit den 70er Jahren im Gebrauch und aus der Notwendigkeit entstanden, Daten zwischen unterschiedlichen Programmen auszutauschen. Alternativen zu CSV sind JSON, XML und Datenbanken. Jede Zeile in einer CSV-Datei entspricht einem Datensatz, während jede Spalte einem Datenfeld entspricht. Zellen können durch Kommata getrennt sein, Variationen benutzen jedoch oft Semikolons, vor allem in Regionen, in denen das Komma als Dezimaltrennzeichen dient.

## See Also
- CSV Crate Dokumentation: https://docs.rs/csv
- Serde: Bibliothek für das Serialisieren und Deserialisieren von Daten – https://serde.rs
- Einführung in Rust: https://www.rust-lang.org/learn
- CSV spezifische Richtlinien der IETF: https://tools.ietf.org/html/rfc4180