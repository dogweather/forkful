---
title:    "Rust: Beregning av datoer i fremtiden eller fortiden"
keywords: ["Rust"]
---

{{< edit_this_page >}}

## Hvorfor
I Rust programmeringsspråket trenger vi ofte å beregne datoer i fremtiden eller fortiden for å lage pålitelige og nøyaktige programmer. Enten det er for å planlegge fremtidige oppgaver eller analysere historiske data, er det viktig å kunne beregne datoer riktig. I denne bloggposten vil vi se på hvordan man kan gjøre dette i Rust.

## Hvordan
Først må vi importere biblioteket `chrono` som lar oss håndtere datoer og tider i Rust. Deretter kan vi bruke funksjonen `today()` for å hente dagens dato, og `days_from_now()` og `days_ago()` for å beregne datoer i fremtiden og fortiden, henholdsvis. Her er et eksempel på hvordan dette kan se ut:

```rust
use chrono::{Local, Duration};

// Henter dagens dato
let today = Local::now();

// Beregner en dato 10 dager frem i tid
let future_date = today + Duration::days(10);

// Beregner en dato 5 dager tilbake i tid
let past_date = today - Duration::days(5);

println!("{:#?}", future_date);
println!("{:#?}", past_date);
```

Dette vil gi følgende utskrift:

```
2020-09-06 13:02:50.389969559 +0200
2020-08-22 13:02:50.389969559 +0200
```

Som du kan se, er datoen formatert på en standardisert måte med tidssone og presisjon. Man kan også beregne datoer basert på timer, minutter, sekunder og millisekunder ved å bruke tilsvarende funksjoner og metoder.

## Dype dykk
I bakgrunnen bruker `chrono` biblioteket `SystemTime` og `Duration` for å håndtere datoer og tider. Disse er implementert i Rusts standardbibliotek, og bruker systemets klokke for å lese og manipulere tid. Dette gjør det mulig å hente nøyaktige datoer og tider uavhengig av tidssoner.

En viktig ting å merke seg er at datoer og tider i Rust er immutabel og ikke kan endres direkte. Dette betyr at når man beregner en dato i fremtiden eller fortiden, vil en ny datoobjekt bli opprettet. Dette sikrer dataintegritet og forhindrer utilsiktet manipulering av datoer.

## Se også
- [Offisiell dokumentasjon for `chrono` biblioteket på crates.io](https://crates.io/crates/chrono)
- [Offisiell dokumentasjon for Rusts standardbibliotek](https://doc.rust-lang.org/std/index.html)
- [Tutorial om bruk av datoer og tider i Rust](https://blog.openshift.com/using-dates-and-times-in-rust/)
- [Guide for å begynne med Rust-programmering](https://dev.to/gjemblaz/start-learning-rust-now-4o3b)