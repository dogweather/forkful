---
title:    "Rust: Konvertere en dato til en streng"
keywords: ["Rust"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/no/rust/converting-a-date-into-a-string.md"
---

{{< edit_this_page >}}

## Hvorfor

Å konvertere en dato til en tekststreng er et vanlig behov for mange programmerere, spesielt når man jobber med datoer og tidsstempel. Dette gjøres for å kunne presentere datoer på en mer lesbar måte for brukerne av applikasjonen.

## Slik gjør du det

I Rust kan man enkelt konvertere en dato til en tekststreng ved å bruke funksjonen `to_string` og deretter velge ønsket format for datoen. La oss si at vi har en dato som er lagret som en `DateTime<Utc>` fra Chrono biblioteket, og vi ønsker å konvertere den til en tekststreng i formatet `dd/mm/yyyy`. Dette kan gjøres på følgende måte:

```rust
use chrono::prelude::*;

let dato = Utc::now(); // hent nåværende dato og tidsstempel
let formatert_dato = dato.format("%d/%m/%Y").to_string();

println!("{}", formatert_dato);
```

Dette vil gi følgende utskrift: `08/10/2021`.

Det er også mulig å endre formatet basert på brukerens preferanser, ved å la de velge mellom flere formater og deretter bruke `match` uttrykk for å velge riktig format.

## Dypdykk

For å forstå hvordan dette fungerer i Rust, må man først ha en forståelse for dato og tidsstempelrepresentasjon i programmering, spesielt i Rust. Datoer og tidsstempler lagres vanligvis som tallverdier, og det er derfor nødvendig å formatere disse tallene til mer leselige tekststrenger. Chrono biblioteket tilbyr et bredt utvalg av formateringsmuligheter, og man kan også lage sine egne tilpassede formater.

## Se også

- [Chrono dokumentasjon](https://docs.rs/chrono/0.4.19/chrono/)
- [Rust DateTime struct dokumentasjon](https://doc.rust-lang.org/std/time/struct.DateTime.html)
- [Rust-Serde bibliotek for å konvertere dato og tidsstempler til JSON](https://serde.rs/)

Vi håper dette innlegget har vært nyttig for å lære hvordan man konverterer datoer til tekst i Rust. Lykke til med programmeringen! 🚀