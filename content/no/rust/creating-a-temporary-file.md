---
title:                "Rust: Oppretting av en midlertidig fil"
programming_language: "Rust"
category:             "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/no/rust/creating-a-temporary-file.md"
---

{{< edit_this_page >}}

## Hvorfor

Noen ganger, når du skriver kode i Rust, må du lage midlertidige filer i løpet av utførelsen av programmet ditt. Dette kan være nødvendig av forskjellige årsaker, for eksempel å lagre midlertidige data eller å kommunisere med andre programmer.

## Hvordan du lager midlertidige filer i Rust

For å lage en midlertidig fil i Rust, kan du bruke `tempfile` biblioteket. Først må du legge til dette biblioteket i avhengighetslisten din ved å legge til følgende linje i `Cargo.toml`-filen din:

```
[dependencies]
tempfile = "3.1.0"
```

Deretter kan du bruke `tempfile::tempfile()`-funksjonen for å opprette en midlertidig fil og få en `File`-peker til den. Det er viktig å merke seg at denne filen automatisk blir slettet når den følgeren går ut av omfanget, så du trenger ikke å bekymre deg for å slette den selv.

Her er et eksempel på hvordan du oppretter en midlertidig fil og skriver noen data til den:

```rust
use tempfile::tempfile;
use std::io::prelude::*;

let mut tmp_file = tempfile().expect("Kunne ikke opprette midlertidig fil");

// Skriv noen data til filen
tmp_file.write_all(b"Dette er noen testdata").expect("Kunne ikke skrive til filen");

// Les data fra filen
let mut buffer = String::new();
tmp_file.read_to_string(&mut buffer).expect("Kunne ikke lese filen");

println!("Innholdet av filen er: {}", buffer);
```

Når denne koden kjøres, vil du se utdata som ligner på følgende:

```
Innholdet av filen er: Dette er noen testdata
```

## Dypere dykk 

Midlertidige filer i Rust er implementert ved hjelp av operativsystemets midlertidige filfunksjonalitet. Dette betyr at filene faktisk blir opprettet på filsystemet, men de er merket som midlertidige og blir automatisk slettet når de ikke lenger er i bruk.

I tillegg til `tempfile`-funksjonen, tilbyr `tempfile`-biblioteket også andre metoder for å opprette midlertidige filer. For eksempel kan du bruke `tempfile::Builder` for å tilpasse hvordan den midlertidige filen skal opprettes.

For å lære mer om hvordan Rust håndterer midlertidige filer og hva som skjer under panseret, kan du lese dokumentasjonen for `tempfile`-biblioteket og lese koden der det er tilgjengelig på GitHub.

## Se også

- [tempfile dokumentasjon](https://docs.rs/tempfile/3.1.0/tempfile/index.html)
- [tempfile GitHub repo](https://github.com/Stebalien/tempfile)