---
title:                "Rust: Beregning av en dato i fremtiden eller fortiden"
programming_language: "Rust"
category:             "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/no/rust/calculating-a-date-in-the-future-or-past.md"
---

{{< edit_this_page >}}

## Hvorfor
Å beregne en dato i fremtiden eller fortiden kan være nyttig for å planlegge hendelser eller få oversikt over tidligere hendelser. Det kan også være en god måte å øve på å bruke Rust-programmeringsspråket på.

## Slik Gjør du Det
For å beregne en dato i Rust, kan du bruke biblioteket "chrono". Først må du legge til dette biblioteket i prosjektet ditt ved å legge til følgende linje i "Cargo.toml" filen:

```
[dependencies]
chrono = "0.4"
```

Deretter kan du importere biblioteket i Rust-filen din ved å legge til følgende linje øverst i filen din:

```
use chrono::{NaiveDate, Datelike, Timelike, Weekday, NaiveDateTime};
```

La oss si at vi vil beregne datoen 100 dager fra i dag. Først må vi opprette en variabel som representerer i dag ved hjelp av "Local" funksjonen fra "chrono" biblioteket:

```
let today = Local::today();
```

Deretter kan vi bruke "checked_add_signed" funksjonen fra "Secs" strukturen for å legge til 100 dager på dagens dato:

```
let future_date = today.checked_add_signed(chrono::Duration::days(100));
```

Til slutt kan du skrive ut datoen på et lesbart format ved å bruke "format" funksjonen:

```
println!("100 dager fra i dag vil være {}", future_date.format("%A, %e %B %Y"));
```

Dette vil resultere i følgende utdat:

```
100 dager fra i dag vil være Saturday, 10 September 2022
```

Du kan også beregne en dato i fortiden ved å bruke "checked_sub_signed" funksjonen og trekke fra et antall dager i stedet for å legge til. Du kan leke rundt med bokstavene i "format" funksjonen for å få ønsket utdataformat.

## Dypdykk
For å forstå bedre hvordan "chrono" biblioteket fungerer, kan du utforske dokumentasjonen og se på koden til de forskjellige funksjonene. Du kan også se nærmere på hvordan datoer og tider representeres i "chrono" ved å bruke de forskjellige strukturene som "NaiveDate", "NaiveDateTime" og "Secs".

## Se Også
- [chrono dokumentasjon](https://docs.rs/chrono/0.4.19/chrono/)
- [Rust programmeringsspråk](https://www.rust-lang.org/no)