---
title:                "Sammenlikning av to datoer"
date:                  2024-01-20T17:34:04.892679-07:00
model:                 gpt-4-1106-preview
simple_title:         "Sammenlikning av to datoer"
programming_language: "Rust"
category:             "Rust"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/no/rust/comparing-two-dates.md"
---

{{< edit_this_page >}}

## Hva & Hvorfor?
Sammenligning av to datoer ser på forskjellen mellom dem — enten i tid, rekkefølge, eller begge deler. Programmerere bruker dette til å håndtere frister, beregne tid som har gått, og organisere hendelser.

## Hvordan:
```Rust
use chrono::{DateTime, Utc};

fn main() {
    let start_dato = Utc.ymd(2023, 3, 18).and_hms(8, 0, 0); // 18. mars 2023, kl 08:00
    let slutt_dato = Utc.ymd(2023, 4, 5).and_hms(17, 30, 0); // 5. april 2023, kl 17:30
    println!("Starter før slutter? {}", start_dato < slutt_dato); 
    println!("Antall dager mellom: {}", (slutt_dato - start_dato).num_days());
}

// Utdata:
// Starter før slutter? true
// Antall dager mellom: 18
```

## Dypdykk
Før `chrono` ble Rust-standard for dato/tidsoperasjoner, brukte folk ofte tid-modulen. `chrono` tilbyr flere funksjoner og bedre sikkerhet. Når du sammenligner datoer, jobber `chrono` med `DateTime`-objekter som kan sammenlignes ved hjelp av standardoperasjonene. Det håndterer også tidsregning og tidssoner på en måte som gjør koden din mer robust.

Du kan også bruke `Duration` for å måle tidsintervaller. Under panseret bruker `chrono` et system med `NaiveDateTime` for datoer og tider uten tidssoneinformasjon og fancy beregninger for skuddår og tidsendringer.

## Se også
- Chrono-dokumentasjon: [https://docs.rs/chrono/latest/chrono/](https://docs.rs/chrono/latest/chrono/)
- Tid-biblioteket før Chrono: [https://docs.rs/time/0.1.43/time/](https://docs.rs/time/0.1.43/time/)