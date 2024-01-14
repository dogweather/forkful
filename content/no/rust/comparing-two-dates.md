---
title:                "Rust: Sammenligning av to datoer"
programming_language: "Rust"
category:             "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/no/rust/comparing-two-dates.md"
---

{{< edit_this_page >}}

## Hvorfor

Mange programmerere står overfor utfordringen med å sammenligne to datoer. Dette kan være nyttig for å bestemme datoforskjeller eller sjekke om en dato er før eller etter en annen. Med Rust-programmeringsspråket kan du effektivt sammenligne datoer ved å bruke den innebygde bibliotekfunksjonen "chrono". I denne bloggposten vil vi se nærmere på hvordan man sammenligner datoer i Rust.

## Hvordan

For å sammenligne datoer i Rust, må vi først importere "chrono" biblioteket ved å legge til følgende kode i begynnelsen av filen:

```Rust
extern crate chrono;

use chrono::{Datelike, NaiveDate};
```

Nå kan vi bruke "NaiveDate" struct for å opprette nye datoer og "Datelike" trait for å sammenligne dem. La oss si at vi ønsker å sammenligne 1. januar 2020 og 1. februar 2020. Vi kan gjøre det som følger:

```Rust
let first_date = NaiveDate::from_ymd(2020, 1, 1); // Oppretter første dato
let second_date = NaiveDate::from_ymd(2020, 2, 1); // Oppretter andre dato

// Sammenligner datoene
if first_date < second_date {
    println!("{} er før {}", first_date, second_date);
} else if first_date > second_date {
    println!("{} er etter {}", first_date, second_date);
} else {
    println!("{} er den samme som {}", first_date, second_date);
}
```

I dette tilfellet vil koden skrive ut "2020-01-01 er før 2020-02-01". 

## Dypdykk

Når man sammenligner datoer i Rust, kan det være nyttig å være oppmerksom på at "chrono" biblioteket også har støtte for tidsstempel, tidsintervaller og tidsavstander. Dette kan være nyttig for mer avanserte sammenligninger av datoer. Dokumentasjonen for "chrono" biblioteket inneholder detaljert informasjon om hvordan man bruker disse funksjonene.

## Se også

- [Chrono dokumentasjon](https://docs.rs/chrono/0.4.11/chrono/)
- [Rust språkguide](https://www.rust-lang.org/learn)
- [Sammenligne datoer i andre programmeringsspråk](https://www.geeksforgeeks.org/comparing-two-dates-one-langugae/)