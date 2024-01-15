---
title:                "Sammenligning av to datoer"
html_title:           "Rust: Sammenligning av to datoer"
simple_title:         "Sammenligning av to datoer"
programming_language: "Rust"
category:             "Rust"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/no/rust/comparing-two-dates.md"
---

{{< edit_this_page >}}

## Hvorfor

Du har kanskje lurt på hvordan man kan sammenligne to datoer i Rust? Kanskje du er i ferd med å utvikle en applikasjon som trenger dette funksjonaliteten, eller kanskje du bare er nysgjerrig. Uansett grunn, i denne artikkelen vil vi se nærmere på hvordan du kan gjøre dette på en enkel og effektiv måte ved hjelp av Rusts innebygde funksjoner.

## Hvordan

For å sammenligne to datoer i Rust, bruker vi typen `DateTime<Utc>` fra biblioteket `chrono`. La oss se på et eksempel:

```Rust
use chrono::{DateTime, Utc, NaiveDate};

// Opprett to datoer
let dato1 = NaiveDate::from_ymd(2021, 07, 22);
let dato2 = NaiveDate::from_ymd(2021, 07, 20);

// Konverter datoene til DateTime<Utc> for å sammenligne
let dateTime1 = DateTime::<Utc>::from_utc(dato1.and_hms(0, 0, 0), Utc);
let dateTime2 = DateTime::<Utc>::from_utc(dato2.and_hms(0, 0, 0), Utc);

// Bruk '<' eller '>' operatører for å sammenligne
if dateTime1 < dateTime2 {
    println!("{} er før {}", dato1, dato2);
} else {
    println!("{} er før {}", dato2, dato1);
}
```

I dette eksempelet, bruker vi funksjonen `from_ymd()` for å opprette to datoer. Deretter konverterer vi dem til `DateTime<Utc>` ved å bruke `from_utc()` og bruker deretter de vanlige sammenligningsoperatørene for å sammenligne dem.

Dette er en enkel måte å sammenligne datoer på, men husk å alltid konvertere datoene til `DateTime<Utc>` før du sammenligner dem for å unngå uforutsette resultater.

## Dypdykk

Hvis du ønsker å sammenligne datoer på en mer presis måte, kan du bruke funksjonene i `chrono`-biblioteket til å hente ut mer detaljert informasjon. For eksempel kan du bruke funksjonen `num_days_from_ce()` for å få antall dager fra år 0 til den angitte datoen.

Du kan også bruke `format()`-funksjonen for å formatere datoen på ønsket måte, eller `weekday()`-funksjonen for å få navnet på ukedagen.

For å utforske alle funksjonene i `chrono`-biblioteket, kan du ta en titt på dokumentasjonen her: [https://docs.rs/chrono/0.4.19/chrono/](https://docs.rs/chrono/0.4.19/chrono/)

## Se også

- [Rust dokumentasjon](https://www.rust-lang.org/learn)
- [Chrono-dokumentasjon](https://docs.rs/chrono/0.4.19/chrono/)
- [Stack Overflow-post om å sammenligne datoer i Rust](https://stackoverflow.com/questions/37552126/how-to-compare-dates-in-rust)