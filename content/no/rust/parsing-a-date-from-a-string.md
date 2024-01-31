---
title:                "Tolke en dato fra en streng"
date:                  2024-01-20T15:38:35.532488-07:00
simple_title:         "Tolke en dato fra en streng"

tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/no/rust/parsing-a-date-from-a-string.md"
---

{{< edit_this_page >}}

## Hva & Hvorfor?
Parsing av datoer fra strenger lar oss tolke og behandle datoer som data. Programmerere gjør dette for å manipulere, lagre eller formatere datoinformasjon.

## Slik gjør du:
For å parse datoer i Rust, kan biblioteket `chrono` brukes slik:

```Rust
extern crate chrono;
use chrono::{DateTime, NaiveDate, TimeZone, Utc};

fn main() {
    let date_string = "2023-04-05T12:30:45Z";
    let parsed_date: DateTime<Utc> = date_string.parse().expect("Invalid date format!");

    println!("Parsed date and time in UTC: {}", parsed_date);
}
```
Kjører du koden, vil du se:
```
Parsed date and time in UTC: 2023-04-05 12:30:45 UTC
```

## Dypdykk
I de tidlige dagene av programmering ble datoer ofte behandlet som enkle tekststrenger, men parsing ble nødvendig for effektiv sortering, manipulering og lagring i databaser. Rust's `chrono` bibliotek er inspirert av moderne C++'s `chrono` bibliotek og Pythons `datetime`. Det tilbyr robuste verktøy for tid- og datoomregninger.

Alternative måter å parse datoer på kunne være med standardbiblioteket `time`, men det er mindre fleksibelt og har begrenset funksjonalitet sammenlignet med `chrono`. Parsing med `chrono` er streng basert og kan enkelt håndtere ulike tidssoner, egendefinerte formater og feilhåndtering.

## Se Også
- Rust `chrono` dokumentasjon: https://docs.rs/chrono/
- Rust `time` dokumentasjon: https://doc.rust-lang.org/std/time/
- Rust Date og Time how-to: https://rust-lang-nursery.github.io/rust-cookbook/datetime.html
