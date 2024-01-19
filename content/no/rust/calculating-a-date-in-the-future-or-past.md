---
title:                "Beregning av en dato i fremtiden eller fortiden"
html_title:           "Rust: Beregning av en dato i fremtiden eller fortiden"
simple_title:         "Beregning av en dato i fremtiden eller fortiden"
programming_language: "Rust"
category:             "Rust"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/no/rust/calculating-a-date-in-the-future-or-past.md"
---

{{< edit_this_page >}}

## Hva & Hvorfor?

Å beregne en dato i fremtiden eller fortiden betyr å utføre beregninger på en dato for å oppdage en annen dato. Dette er populært blant programmerere for å håndtere oppgaver som fremdriftssporing, påminnelser og planlegging av oppgaver.

## Hvordan du:

Vi skal bruke crate `chrono` som krever at du legger til følgende avhengighet til din Cargo.toml.

```Rust
[dependencies]
chrono = "0.4.19"
```

Her er en enkel kode snutt som viser deg hvordan du kan få en dato 30 dager fra i dag.

```Rust
use chrono::{DateTime, Duration, Utc};

fn main() {
    let now: DateTime<Utc> = Utc::now();
    let future: DateTime<Utc> = now + Duration::days(30);
    println!("{}", future);
}
```
Når du kjører denne koden vil du se noe lignende i output:

```Rust
2023-09-04 14:56:09.051918 UTC
```

Og å beregne en dato i fortiden kunne ikke vært enklere. Her er hvordan:

```Rust
use chrono::{DateTime, Duration, Utc};

fn main() {
    let now: DateTime<Utc> = Utc::now();
    let past: DateTime<Utc> = now - Duration::days(30);
    println!("{}", past);
}
```
Output fra kodebiten, vil gi deg noe lignende:

```Rust
2023-07-06 14:56:09.051918 UTC
```

## Dypdykk

Historisk sett har håndtering av datoer og tid alltid vært en stor utfordring i programmering. Problemene varierte fra håndtering av tidssoner til skuddår. Biblioteket `chrono` i Rust hjelper deg med å løse disse kompliserte problemene.

Alternativene til `chrono` inkluderer `time` og `date` crate, men mangler ofte `chrono` sin brukervennlighet og funksjonsdybde.

Viktig å merke seg er at `chrono` gjør det enkelt å manipulere datoer ved hjelp av `Duration` klassen, som håndterer tillegg og subtraksjon av dager, ukedager, sekunder osv., mens stiller hensyn til viktige detaljer som skuddsekunder og tidsråsonering.

## Se Også

For mer informasjon, sjekk ut følgende ressurser:

- `chrono` dokumentasjon: https://docs.rs/chrono/0.4.19/chrono/
- Rust dato og tidshåndtering: https://stevedonovan.github.io/rustifications/2018/09/08/common-rust-lifetime-misconceptions.html
- Alternativer til `chrono`: https://lib.rs/crates/time