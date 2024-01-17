---
title:                "Jobbe med CSV"
html_title:           "Rust: Jobbe med CSV"
simple_title:         "Jobbe med CSV"
programming_language: "Rust"
category:             "Rust"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/no/rust/working-with-csv.md"
---

{{< edit_this_page >}}

Hva og hvorfor?

Arbeider du med store mengder data og trenger en måte å organisere og lagre informasjonen på? Da kan du ha nytte av CSV, som står for Comma-Separated Values. Dette er et vanlig format for å lagre tabellarisk data, og brukes ofte i programmering for å enkelt importere og eksportere data.

Hvordan:

Her er et eksempel på hvordan du kan lese en CSV-fil og skrive ut dataene i Rust:

```Rust
use std::fs::File;
use std::io::{self, BufReader};
use std::error::Error;
use csv::ReaderBuilder;

fn main() -> Result<(), Box<dyn Error>> {
    // Åpne CSV-fil
    let file = File::open("data.csv")?;
    // Les fra filen
    let reader = BufReader::new(file);
    // Lag en CSV-leser med standardinnstillingene
    let mut csv_reader = ReaderBuilder::new().has_headers(true).from_reader(reader);
    // Iterer gjennom hver linje i filen
    for result in csv_reader.records() {
        // Håndter feil i lesingen av filen
        let record = result?;
        // Skriv ut dataene
        println!("{:?}", record);
    }
    Ok(())
}
```

Dette vil skrive ut hver linje i CSV-filen som en vektor med verdier. For eksempel, hvis filen inneholder "Navn, Alder, Land" som overskrifter og "Lars, 25, Norge" som data, vil outputen bli: ```["Lars", "25", "Norge"]```. Du kan deretter bruke disse verdiene til å utføre flere handlinger i programmet ditt.

Dypdykk:

CSV-formatet har eksistert siden 1970-tallet og har blitt viktig i både dataanalyse og programmering. Det finnes også alternative formater for å lagre tabellarisk data, som JSON og XML, men CSV er ofte enklere å arbeide med fordi det er enklere å lese og skrive.

Implementasjonen av CSV i Rust er gjort gjennom biblioteket "csv", som tilbyr forskjellige innstillinger og funksjonaliteter for å arbeide med CSV-data. Det er også enkelt å integrere med andre Rust-biblioteker og å skrive egne funksjoner for å håndtere CSV-data på en tilpasset måte.

Se også:

- Rust sin offisielle dokumentasjon for CSV-biblioteket: https://docs.rs/csv/
- En guide til å arbeide med CSV i Rust: https://rust-lang-nursery.github.io/rust-cookbook/data/import_data/csv.html