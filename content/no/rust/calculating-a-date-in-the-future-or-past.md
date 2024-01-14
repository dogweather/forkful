---
title:    "Rust: Beregning av en dato i fremtiden eller fortiden"
keywords: ["Rust"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/no/rust/calculating-a-date-in-the-future-or-past.md"
---

{{< edit_this_page >}}

## Hvorfor
Hvorfor kunne vi trenge å beregne en dato i fremtiden eller fortiden? Det kan være nyttig for å planlegge fremtidige hendelser eller for å gjenopprette informasjon om tidligere hendelser.

## Hvordan
For å beregne en dato i Rust, kan vi bruke biblioteket "chrono". Vi må starte med å legge til følgende linje i "Cargo.toml" filen:
```Rust
[dependencies]
chrono = "0.4"
```
Deretter kan vi importere biblioteket i koden vår:
```Rust
use chrono::{UTC, Duration, DateTime};
```
For å beregne en dato i fremtiden, kan vi bruke "UTC" funksjonen for å få dagens dato, og deretter legge til en "Duration" for å angi hvor langt frem i tid vi vil beregne:
```Rust
let now = UTC::now();
let future_date = now + Duration::weeks(2);
println!("Datoen to uker fra nå er {}", future_date);
```
Dette vil gi oss følgende output:
```
Datoen to uker fra nå er 2020-08-31 14:30:00 UTC
```
Vi kan også beregne en dato i fortiden ved å bruke "Duration" med et negativt tall:
```Rust
let now = UTC::now();
let past_date = now + Duration::days(-7);
println!("Datoen en uke tilbake var {}", past_date);
```
Dette vil gi oss følgende output:
```
Datoen en uke tilbake var 2020-08-10 14:30:00 UTC
```

## Dypdykk
Her er noen ting å merke seg når vi beregner datoer i Rust:
- "chrono" biblioteket bruker UTC-tid, så det er viktig å være oppmerksom på eventuelle lokale tidssoneforskjeller.
- Vi kan også bruke "Duration" til å beregne tidsintervaller i timer, minutter, sekunder osv.
- Det finnes andre nyttige funksjoner og metoder i "chrono" biblioteket, som for eksempel å konvertere datoer til forskjellige formater.

## Se også
- [Rust dokumentasjon for "chrono" biblioteket](https://docs.rs/chrono/0.4.19/chrono/)
- [Offisielt Rust forum (på norsk)](https://users.rust-lang.org/c/internasjonalt-svorsk-forum/)
- [Et annet blogginnlegg som utforsker "chrono" biblioteket](https://steveklabnik.github.io/chrono/chrono/)