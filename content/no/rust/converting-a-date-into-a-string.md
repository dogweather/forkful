---
title:                "Konvertering av en dato til en streng"
html_title:           "Rust: Konvertering av en dato til en streng"
simple_title:         "Konvertering av en dato til en streng"
programming_language: "Rust"
category:             "Rust"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/no/rust/converting-a-date-into-a-string.md"
---

{{< edit_this_page >}}

## Hvorfor
Det å konvertere et datoobjekt til en streng kan være svært nyttig når man jobber med datoer og tidsstemplinger i Rust. Det kan hjelpe deg med å presentere datoer på en mer lesbar måte i brukergrensesnitt eller lagre dem i en database.

## Hvordan
For å konvertere en dato til en streng i Rust, kan du bruke `to_string` metoden til datoobjektet. La oss si at vi har et datoobjekt som ser slik ut: `let dato = NaiveDate::from_ymd(2021, 9, 15);`

Nå kan vi enkelt konvertere dette til en streng ved å bruke `to_string` metoden slik: `let dato_string = dato.to_string();`

Her er et eksempel på hvordan dette fungerer i praksis:

```rust
use chrono::prelude::*;
fn main() {
    let dato = NaiveDate::from_ymd(2021, 9, 15);
    let dato_string = dato.to_string();
    println!("{}", dato_string);
}
```

Kjører dette programmet vil gi følgende output: `2021-09-15`

Dette er den standardformaten for datoer i Rust, men du kan også bruke `format` metoden for å tilpasse formatet. For eksempel, hvis du ønsker å vise datoen som `15.09.2021`, kan du bruke følgende kode:

```rust
use chrono::prelude::*;
fn main() {
    let dato = NaiveDate::from_ymd(2021, 9, 15);
    let dato_string = dato.format("%d.%m.%Y").to_string();
    println!("{}", dato_string);
}
```

Kjører dette vil gi følgende output: `15.09.2021`

## Dypdykk
Det finnes flere måter å konvertere et datoobjekt til en streng på i Rust, avhengig av hvilket format du ønsker å bruke. I eksemplene over har vi brukt `to_string` og `format` metoden fra Chrono biblioteket. Det finnes også andre biblioteker som tilbyr lignende funksjonalitet, som for eksempel `strftime` fra time biblioteket.

Det er også viktig å være klar over at hvis du ønsker å konvertere datoer til en spesifikk tidsone, må du bruke `with_timezone` metoden sammen med `to_string` eller `format` for å få riktig resultat.

## Se også
- [Chrono dokumentasjon](https://docs.rs/chrono/0.4.19/chrono/)
- [Rust Standard Library - Dates and Times](https://doc.rust-lang.org/std/time/index.html)
- [Time biblioteket](https://crates.io/crates/time)