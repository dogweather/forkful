---
title:                "Werken met CSV"
date:                  2024-01-28T22:10:52.501639-07:00
model:                 gpt-4-0125-preview
simple_title:         "Werken met CSV"

category:             "Rust"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/nl/rust/working-with-csv.md"
changelog:
  - 2024-01-28, gpt-4-0125-preview, translated from English
---

{{< edit_this_page >}}

## Wat & Waarom?

CSV, de afkorting voor Comma-Separated Values (komma-gescheiden waarden), is een bestandsformaat dat wordt gebruikt om tabelgegevens op te slaan. Programmeurs houden van CSV vanwege de eenvoud en de brede ondersteuning in tools en programmeertalen voor gegevensmanipulatie, import en export.

## Hoe te:

Voeg eerst de benodigde crate toe aan `Cargo.toml`:

```toml
[dependencies]
csv = "1.1"
```

Behandel vervolgens het lezen van een CSV:

```rust
use csv::Reader;
use std::error::Error;

fn main() -> Result<(), Box<dyn Error>> {
    let mut rdr = Reader::from_path("data.csv")?;
    for resultaat in rdr.records() {
        let record = resultaat?;
        println!("{:?}", record);
    }
    Ok(())
}
```

Schrijf naar een CSV:

```rust
use csv::Writer;
use std::error::Error;

fn main() -> Result<(), Box<dyn Error>> {
    let mut wtr = Writer::from_path("output.csv")?;
    wtr.write_record(&["naam", "stad", "leeftijd"])?;
    wtr.write_record(&["Jane", "New York", "30"])?;
    wtr.flush()?;
    Ok(())
}
```

Voorbeelduitvoer voor lezen:

```
StringRecord(["Jane", "New York", "30"])
```

## Diepgaande duik

CSV bestaat al sinds de vroege dagen van persoonlijke computers en werd gebruikt voor het uitwisselen van gegevens tussen programma's en systemen. Terwijl JSON en XML meer structuur bieden, blijft CSV populair vanwege zijn lichte gewicht en gebruiksgemak.

Alternatieven voor de csv crate in Rust zijn onder andere `serde_csv`, dat handige serialisatie en deserialisatie biedt, en `papercut`, dat zich richt op veilige en ergonomische CSV-analyse.

CSV-analyse in Rust is I/O-gebonden. Efficiënte afhandeling betreft het gebruik van iterators en de robuuste foutafhandeling van Rust om slecht gevormde gegevens te beheren.

## Zie ook

- Rust CSV crate documentatie: https://docs.rs/csv/
- Het Rust Programmeringstaal boek: https://doc.rust-lang.org/book/
- Serde: https://serde.rs/ - een framework voor het serialiseren en deserialiseren van Rust-gegevensstructuren.
- Rust bij Voorbeeld CSV: https://rustbyexample.com/std_misc/file/csv.html
