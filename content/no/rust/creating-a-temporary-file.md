---
title:    "Rust: Opprette en midlertidig fil"
keywords: ["Rust"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/no/rust/creating-a-temporary-file.md"
---

{{< edit_this_page >}}

## Hvorfor

Hvis du noen gang har jobbet med datafiler, programvaretesting eller generelt systemarbeid, har du sannsynligvis støtt på behovet for å opprette midlertidige filer. Disse filene er midlertidige lagringsplasser som brukes for å mellomlagre data mens du jobber med et større prosjekt. Å opprette og håndtere midlertidige filer i dine Rust-prosjekter kan være en viktig del av effektiviteten og funksjonaliteten til programvaren din.

## Hvordan

For å opprette en midlertidig fil i Rust, kan du bruke `tempfile` biblioteket. Først må du legge til biblioteket i `Cargo.toml` filen din:

```Rust
[dependencies]
tempfile = "3.1.0"
```

Deretter importerer du biblioteket i filen din ved å legge til følgende i toppen av koden:

```Rust
extern crate tempfile;
```

For å opprette en midlertidig fil kan du bruke `NamedTempFile`-strukturen i `tempfile` biblioteket:

```Rust
use std::io::prelude::*;
use tempfile::NamedTempFile;

let mut file = NamedTempFile::new().expect("Kunne ikke opprette midlertidig fil");
```

Deretter kan du skrive data til filen ved hjelp av `write`-metoden:

```Rust
let data = b"Dette er et eksempel på data som skal skrives til midlertidig fil.";
file.write(data).expect("Kunne ikke skrive til fil");
```

For å lese data fra filen, kan du bruke `read_to_string`-metoden:

```Rust
let mut contents = String::new();
file.read_to_string(&mut contents).expect("Kunne ikke lese fra fil");
println!("{}", contents); // Skriver ut "Dette er et eksempel på data som skal skrives til midlertidig fil."
```

## Dykk dypere

Opprettelsen av midlertidige filer i Rust er enkelt og kan være nyttig i en rekke forskjellige programmeringsprosjekter. Det er også viktig å merke seg at `NamedTempFile`-strukturen har flere nyttige metoder, som for eksempel `persist` som gir deg muligheten til å beholde den midlertidige filen permanent dersom du ønsker det.

Det er også verdt å nevne at `tempfile` biblioteket også støtter opprettelse av midlertidige mapper og filer med spesifikke navn og plasseringer. For mer informasjon kan du se på dokumentasjonen til biblioteket.

## Se også

- [rust-lang.org](https://www.rust-lang.org/)
- [tempfile dokumentasjon](https://docs.rs/tempfile/3.1.0/tempfile/)
- [Rust-referanse](https://doc.rust-lang.org/reference/index.html)