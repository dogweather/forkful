---
title:                "Arbeid med csv"
html_title:           "Rust: Arbeid med csv"
simple_title:         "Arbeid med csv"
programming_language: "Rust"
category:             "Rust"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/no/rust/working-with-csv.md"
---

{{< edit_this_page >}}

## Hvorfor

CSV (Comma-Separated Values) er en vanlig filformat som brukes til å lagre og transportere tabelldata. Det er et nyttig verktøy for alle som jobber med data og ønsker å organisere og analysere informasjon på en effektiv måte. I denne artikkelen skal vi se på hvordan man kan bruke Rust til å håndtere CSV-filer, noe som kan være spesielt nyttig for utviklere som jobber med store datasett.

## Hvordan

```rust
use std::error::Error;
use csv::ReaderBuilder;

fn main() -> Result<(), Box<dyn Error>> {
    // Opprett en lesbar CSV-leser
    let mut reader = ReaderBuilder::new().from_path("data.csv")?;

    // Les data fra CSV-filen rad for rad
    for result in reader.records() {
        // Hent ut raden som en vektor med strenger
        let record = result?;

        // Hent ut data fra bestemte kolonner
        let first_name = record[0];
        let last_name = record[1];
        let age = record[2].parse::<i32>()?;

        // Gjør noe med dataen, for eksempel skriv den ut
        println!("{} {} er {} år gammel.", first_name, last_name, age);
    }

    Ok(())
}
```

Eksempel på CSV-fil (data.csv):

```csv
John,Smith,32
Emily,Johnson,28
Michael,Williams,47
```

Output:

```
John Smith er 32 år gammel.
Emily Johnson er 28 år gammel.
Michael Williams er 47 år gammel.
```

## Dypdykk

Rust tilbyr et bibliotek kalt "csv" som gjør det enkelt å lese og skrive til CSV-filer. Dette biblioteket håndterer de fleste av de vanlige CSV-formatene og støtter også muligheten til å tilpasse lesing og skriving av data. Det betyr at du kan tilpasse koden din for å håndtere spesifikke utfordringer som måtte oppstå når du jobber med CSV-filer.

En annen fordel med Rust er at det er et språk som er kjent for å være pålitelig og effektivt. Dette er viktig når man jobber med store datasett, da man ønsker at programmet skal kjøre raskt og feilfritt. Rusts strenge typetilordning og kontroll over minnehåndtering gjør det til et godt valg for å håndtere komplekse datastrukturer som CSV-filer.

## Se også

- [csv dokumentasjon](https://docs.rs/csv/latest/csv/)
- [Rust offisiell nettside](https://www.rust-lang.org/no)
- [Rust Community](https://www.rust-lang.org/no/community)

Håper denne artikkelen har gitt deg et innblikk i hvordan du kan bruke Rust til å jobbe med CSV-filer, og hvordan dette språket kan være nyttig for å håndtere store datasett. Ikke nøl med å utforske mulighetene og utvide kunnskapen din om Rust og dets funksjoner. Lykke til!