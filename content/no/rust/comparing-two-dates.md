---
title:                "Rust: Sammenligning av to datoer"
simple_title:         "Sammenligning av to datoer"
programming_language: "Rust"
category:             "Rust"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/no/rust/comparing-two-dates.md"
---

{{< edit_this_page >}}

# Hvorfor

I programmering er det viktig å kunne sammenligne og operere med ulike typer data. En viktig del av dette er å sammenligne to datoer. Det kan være nyttig for å sortere data, finne ut når en hendelse skjedde eller for å sjekke om en dato er før eller etter en annen. I denne artikkelen vil vi utforske hvordan man kan gjøre dette i Rust-programmeringsspråket.

# Hvordan

For å sammenligne to datoer i Rust trenger vi å bruke et bibliotek kalt "chrono". Først må vi importere biblioteket ved å legge det til i prosjektets avhengigheter. Dette kan gjøres i din Cargo.toml-fil ved å legge til følgende linje under [dependencies]:

```rust
chrono = { version = "0.4", features = ["serde"] }
```

Vi må også importere biblioteket i selve koden vår ved å legge til følgende linje:

```rust
use chrono::{DateTime, Utc};
```

Nå kan vi begynne å sammenligne to datoer. La oss starte med å opprette to DateTime-objekter, en for hver dato:

```rust
let dato1 = Utc.ymd(2022, 6, 26).and_hms(9, 0, 0);
let dato2 = Utc.ymd(2021, 1, 1).and_hms(12, 30, 0);
```

Så kan vi sammenligne de to datoene ved å bruke operatorer som er tilgjengelige fra chrono-biblioteket. For eksempel kan vi se om dato2 er før dato1 ved å skrive:

```rust
if dato2 < dato1 {
    println!("Dato 2 er før dato 1");
} else {
    println!("Dato 2 er etter dato 1");
}
```

Andre nyttige operatorer for å sammenligne datoer inkluderer "==" (lik), "!=" (ikke lik), ">=" (større eller lik) og "<=" (mindre eller lik).

# Deep Dive

Når vi sammenligner to datoer i Rust, bruker vi egentlig bare operatorer for å sammenligne de indre verdiene av DateTime-objektene. Dette betyr at vi sammenligner årstall, måneder, dager, timer, minutter og sekunder. For å få en dypere forståelse av hvordan sammenligningen fungerer kan vi se på DateTime-objektet som en tidslinje. Årstallet representerer starten av tidslinjen, og alle andre verdier legges til derfra.

Det er også viktig å merke seg at DateTime-objektene tar hensyn til tidsforskjeller og tidssoner. Derfor vil to datoer som ser identiske ut i koden, faktisk kunne være forskjellige i virkeligheten. For å unngå dette kan man bruke Utc-varianten av DateTime, som ignorerer tidssoner og tidsforskjeller.

# Se også

Lær mer om chrono-biblioteket og hvordan det kan brukes til å håndtere datoer i Rust:

- [Chrono Documentation](https://docs.rs/chrono/0.4.19/chrono/)
- [Date and Time Handling in Rust with Chrono](https://www.codementor.io/@isaacyong/date-and-time-handling-in-rust-with-chrono-gnet9qnhw)