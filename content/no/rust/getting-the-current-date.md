---
title:                "Slik får du tak i dagens dato"
date:                  2024-01-20T15:16:48.426005-07:00
html_title:           "C: Slik får du tak i dagens dato"
simple_title:         "Slik får du tak i dagens dato"
programming_language: "Rust"
category:             "Rust"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/no/rust/getting-the-current-date.md"
---

{{< edit_this_page >}}

## Hva & Hvorfor?
Å hente dagens dato i kode betyr å finne ut nøyaktig hvilken dato det er akkurat nå. Programmerere gjør dette for funksjoner som logger, daterer hendelser, eller rett og slett viser dato til brukeren.

## Hvordan gjøre det:
For å hente dagens dato i Rust, bruk `Chrono`-biblioteket, som er robust og greit å jobbe med. Her er et lite eksempel:

```Rust
extern crate chrono;
use chrono::prelude::*;

fn main() {
    let local_date = Local::today();
    println!("{}", local_date.format("%Y-%m-%d"));
}
```

Dette vil gi deg dagens dato, formatert som `"ÅÅÅÅ-MM-DD"`.

## Dypdykk
Før `Chrono` ble favoritten, brukte Rust programmører `time`-biblioteket. `Chrono` gir en mer omfattende løsning for dato- og tidsbehandling. Det støtter tidszoner, ulike kalendere og formattering. Når det gjelder å få dagens dato, så handler det mest om å hente systemtiden og å formatere den på en forståelig måte.

Alternativer til `Chrono` inkluderer `time`-biblioteket eller standardbibliotekets `std::time`, selv om disse kanskje er mindre fleksible eller mer verbøse for noen bruksområder. Til syvende og sist avhenger valget av bibliotek av dine spesifikke behov.

`Chrono` implementerer funksjoner ved å bruke systemklokken og kan representere tid i både UTC og lokale tidszoner. Dette er et viktig aspekt når man arbeider med servere og brukere fra forskjellige steder av verden.

## Se også
- Offisiell `Chrono`-dokumentasjon: [docs.rs/chrono](https://docs.rs/chrono)
- Rust’s dato- og tids-API forklaring: [doc.rust-lang.org/std/time](https://doc.rust-lang.org/std/time/index.html)