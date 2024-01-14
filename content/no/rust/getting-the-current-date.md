---
title:    "Rust: Få gjeldende dato"
keywords: ["Rust"]
---

{{< edit_this_page >}}

## Hvorfor

Det å få den nåværende datoen er et vanlig behov i programmering, spesielt når man arbeider med datoer og tidsstempel. Ved å lære hvordan man gjør dette i Rust, kan du unngå feil og spare tid i koden din. Så hvorfor ikke lære å gjøre det på riktig måte?

## Slik gjør du det

I Rust kan du få den nåværende datoen på flere måter, men den enkleste og mest nøyaktige måten er å bruke biblioteket `chrono`. Først må du legge til følgende linje i din `Cargo.toml` fil:

```
[dependencies]
chrono = "0.4"
```

Deretter kan du importere biblioteket i din Rust-fil ved å skrive:

```
extern crate chrono;

use chrono::prelude::*;
```

Nå kan du bruke `Utc::today()` funksjonen for å få den nåværende datoen i UTC-format:

```
let current_date = Utc::today();
println!("Dagens dato er {}", current_date);
```

Dette vil gi følgende output:

```
Dagens dato er 2021-11-04.
```

Du kan også få den nåværende datoen i lokal tid ved å bruke `Local::today()` funksjonen:

```
let current_date = Local::today();
println!("Dagens dato er {}", current_date);
```

Dette vil gi output som følger:

```
Dagens dato er 2021-11-04.
```

## Dypdykk

Biblioteket `chrono` gir også flere alternativer for å få den nåværende datoen, som for eksempel å få datoen og tiden samtidig. Du kan også formatere datoen på ulike måter ved å bruke `format` funksjonen. Det finnes også andre biblioteker som tilbyr samme funksjonalitet, som for eksempel `time` og `date_time`.Å lære mer om disse kan hjelpe deg å velge det som passer best for ditt spesifikke prosjekt.

## Se også

- [Dokumentasjon for `chrono` biblioteket](https://docs.rs/chrono/0.4.19/chrono/index.html)
- [Dokumentasjon for `time` biblioteket](https://docs.rs/time/0.2.24/time/)
- [Dokumentasjon for `date_time` biblioteket](https://docs.rs/date_time/0.1.0/date_time/)

Takk for at du leste denne bloggposten om å få den nåværende datoen i Rust! Vi håper dette hjalp deg å forstå grunnleggende konsepter og hva biblioteket `chrono` har å tilby. Lykke til med ditt neste Rust-prosjekt!