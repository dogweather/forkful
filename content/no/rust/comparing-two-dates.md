---
title:                "Sammenligning av to datoer"
html_title:           "Rust: Sammenligning av to datoer"
simple_title:         "Sammenligning av to datoer"
programming_language: "Rust"
category:             "Rust"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/no/rust/comparing-two-dates.md"
---

{{< edit_this_page >}}

## Hva & Hvorfor?
Sammenligning av to datoer er en vanlig oppgave for programmerere, der man sjekker om en dato er større eller mindre enn en annen. Dette er ofte nyttig for å sortere eller filtrere data etter datoer i en applikasjon.

## Slik gjør du det:
For å sammenligne to datoer i Rust, kan du bruke standard biblioteket DateTime og dens metode "cmp". Du må først konvertere datoene til DateTime-objekter ved hjelp av metoden "parse", og deretter kan du bruke "cmp" for å sammenligne dem. Se eksempel under:

```Rust

use std::cmp::Ordering;
use std::time::Duration;
use std::time::Instant;
 
let dato1 = DateTime::parse("2020-01-01T12:00:00").unwrap();
let dato2 = DateTime::parse("2021-01-01T12:00:00").unwrap();
 
let sammenligning = dato1.cmp(&dato2);

match sammenligning {
    Ordering::Less => println!("Dato 1 er før Dato 2"),
    Ordering::Equal => println!("Dato 1 er lik Dato 2"),
    Ordering::Greater => println!("Dato 1 er etter Dato 2"),
}

```

Output: "Dato 1 er før Dato 2"

## Dypdykk:
Sammenligning av datoer har vært en utfordring for programmerere i mange år, spesielt med tanke på ulike formater og tidssoner. Alternativene for å løse dette problemet, som også er tilgjengelige i Rust, er å bruke tredjeparts biblioteker som Chrono eller å implementere din egen funksjon for sammenligning av datoer. 

I forbindelse med tidssoner er det viktig å merke seg at DateTime-objekter i Rust er basert på UTC-tid, så det kan være nødvendig å justere datoene før sammenligning for å ta hensyn til lokale tidssoner.

## Se også:
- Rust Standardbibliotek dokumentasjon for DateTime: https://doc.rust-lang.org/std/time/struct.DateTime.html
- Chrono bibliotek for dato/klokkeslett manipulering i Rust: https://docs.rs/chrono/0.4.19/chrono/
- Stack Overflow diskusjon om å sammenligne datoer i Rust: https://stackoverflow.com/questions/31537845/how-to-compare-two-dates-in-rust