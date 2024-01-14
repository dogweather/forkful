---
title:                "Rust: Konvertering av en dato til en streng"
simple_title:         "Konvertering av en dato til en streng"
programming_language: "Rust"
category:             "Rust"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/no/rust/converting-a-date-into-a-string.md"
---

{{< edit_this_page >}}

## Hvorfor

Rust er et kraftig og svært populært programmeringsspråk, og å kunne konvertere en dato til string er en grunnleggende ferdighet for enhver Rust-utvikler. Det lar deg enkelt lese og manipulere datoer i dine programmer og prosjekter.

## Slik gjør du det

For å konvertere en dato til en string i Rust, kan du bruke standard biblioteket `chrono`. Før du begynner, må du legge til `chrono` til avhengighetene dine i `Cargo.toml` filen din:

```Rust
[dependencies]
chrono = "0.4"
```

Deretter kan du importere biblioteket i Rust-filen din ved å legge til følgende linje:

```Rust
use chrono::{DateTime, Datelike, NaiveDate, Utc};
```

Nå kan vi konvertere en dato til en string ved å først opprette en `DateTime`-verdi ved hjelp av `Local::now()`-funksjonen, som returnerer nåværende dato og tid i lokal tidssone. Deretter kan vi bruke `format()`-metoden for å konvertere den til ønsket stringformat.

```Rust
let now = Local::now();
println!("{}", now.format("%Y-%m-%d"));
```

Dette vil resultere i følgende output:

```bash
2021-10-25
```

Det er også en rekke andre formateringsalternativer tilgjengelig, for eksempel `%d.%m.%Y` for å få datoen i formatet "25.10.2021". Du kan også spesifisere tidssone og dato og tid med mer nøyaktighet ved å bruke `Utc::now()` i stedet for `Local::now()`.

## Dypdykk

Nå som du har lært hvordan du konverterer en dato til en string i Rust, kan det være nyttig å dykke litt dypere og se på hvordan `chrono` biblioteket fungerer. Det er basert på konseptet av tre typer datoer: `DateTime`, `Date` og `Time`. `DateTime` er den komplette datoen og tiden, mens `Date` og `Time` representerer bare hver for seg. Dette gjør det mulig å arbeide med disse verdiene separat og legge dem sammen for å få en `DateTime`-verdi.

En annen viktig ting å merke seg er at alle datoer i `chrono` er i UTC-tidssone. Dette sikrer riktig behandling av datoer og tider uansett hvor i verden en bruker befinner seg.

## Se også

- [Chrono dokumentasjon](https://docs.rs/chrono/0.4.19/chrono/)
- [Offisiell Rust nettside](https://www.rust-lang.org/no-NO/)
- [Rust subreddit](https://www.reddit.com/r/rust/)