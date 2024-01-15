---
title:                "Hente nåværende dato"
html_title:           "Rust: Hente nåværende dato"
simple_title:         "Hente nåværende dato"
programming_language: "Rust"
category:             "Rust"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/no/rust/getting-the-current-date.md"
---

{{< edit_this_page >}}

## Hvorfor

Det er mange ulike situasjoner der du kanskje trenger å få den nåværende datoen i et program. For eksempel kan du ønske å registrere når koden din ble kjørt, eller vise datoen i et brukergrensesnitt. Uansett årsak, er det viktig å kunne få tak i den nåværende datoen for å gjøre programmet ditt mer dynamisk og nyttig.

## Hvordan

Det er flere måter å få tak i den nåværende datoen i Rust, avhengig av dine preferanser og behov.

Først kan du bruke biblioteket `chrono`, som tilbyr en enkel måte å håndtere dato og tid på i Rust. Du kan installere dette biblioteket ved å legge til følgende linje under `[dependencies]` i `Cargo.toml` filen din:

```Rust
chrono = "0.4.15"
```

Deretter kan du bruke `chrono::Local::now()`, som vil returnere et `DateTime<Local>` objekt som representerer den nåværende datoen og tiden i ditt lokale tidssone.

```Rust
use chrono::Local;

let now = Local::now();
println!("Dagens dato er: {}", now.format("%d.%m.%Y"));
```

Dette vil printe ut datoen i formatet "dag.måned.år". Du kan også formatere datoen på andre måter ved å endre på `format()` funksjonen.

Du kan også få tak i den nåværende datoen ved hjelp av standardbibliotekets `std::time::SystemTime` type. Dette vil returnere en `SystemTime` struktur med informasjon om den nåværende datoen og tiden.

```Rust
use std::time::SystemTime;
use std::time::UNIX_EPOCH;

let now = SystemTime::now();
let days_since_epoch = now.duration_since(UNIX_EPOCH).expect("Tiden må være etter epoken").as_secs() / (60 * 60 * 24);
println!("Det har gått {} dager siden epoken.", days_since_epoch);
```

Dette vil printe ut antall dager siden 1. januar 1970 (også kjent som epoken). Du kan også konvertere `SystemTime` til en `DateTime` objekt ved hjelp av `to_datetime()` funksjonen og deretter formatere datoen slik du ønsker.

## Dypdykk

Det er viktig å merke seg at både `chrono` og `SystemTime` vil gi deg datoen og tiden i ditt lokale tidssone. Dette kan være problematisk dersom du ønsker å vise den nåværende datoen for en bruker i en annen tidssone. I slike tilfeller kan det være lurt å bruke `Utc::now()` i `chrono` biblioteket for å få datoen og tiden i UTC (koordinert universaltid).

Det er også verdt å nevne at `chrono` ikke støtter tidssoner, bare datokonverteringer. Hvis du trenger å arbeide med ulike tidssoner i ditt program, kan du se på biblioteket `chrono-tz` som utvider funksjonaliteten til `chrono`.

## Se også

- Rusts offisielle dokumentasjon: [https://www.rust-lang.org/](https://www.rust-lang.org/)
- `chrono` biblioteket: [https://docs.rs/chrono/0.4.15/chrono/](https://docs.rs/chrono/0.4.15/chrono/)
- `chrono-tz` biblioteket: [https://docs.rs/chrono-tz/0.5.0/chrono_tz/](https://docs.rs/chrono-tz/0.5.0/chrono_tz/)