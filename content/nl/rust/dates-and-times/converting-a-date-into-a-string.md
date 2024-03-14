---
changelog:
- 2024-01-28, gpt-4-0125-preview, translated from English
date: 2024-01-28 21:57:45.171100-07:00
description: "Het converteren van een datum naar een string in Rust stelt ons in staat\
  \ om data weer te geven in een voor mensen leesbaar formaat. We doen dit voor UI's,\u2026"
lastmod: '2024-03-13T22:44:50.605875-06:00'
model: gpt-4-0125-preview
summary: "Het converteren van een datum naar een string in Rust stelt ons in staat\
  \ om data weer te geven in een voor mensen leesbaar formaat. We doen dit voor UI's,\u2026"
title: Een datum converteren naar een string
---

{{< edit_this_page >}}

## Wat & Waarom?

Het converteren van een datum naar een string in Rust stelt ons in staat om data weer te geven in een voor mensen leesbaar formaat. We doen dit voor UI's, logs of elke plaats waar mensen datums moeten kunnen begrijpen.

## Hoe te:

Rust's `chrono` crate is de aangewezen keuze voor het omgaan met datums en tijden. Zorg ervoor dat het in je `Cargo.toml` staat:

```toml
[dependencies]
chrono = "0.4"
```

Laten we nu een datum formatteren als een string.

```rust
extern crate chrono;
use chrono::{DateTime, Utc, NaiveDateTime};

fn main() {
    let date: DateTime<Utc> = Utc::now(); // Haal de huidige UTC datum en tijd op.
    let formatted_date = date.format("%Y-%m-%d %H:%M:%S").to_string();
    println!("{}", formatted_date); // Print: 2023-03-15 14:30:45
}
```

## Diepgaande Duik

Vóór `chrono` had Rust's standaardbibliotheek een paar datum- en tijd functies, maar die waren basic. `chrono` bouwde op die basis voort om uitgebreide functionaliteit te bieden. Een alternatief zou Rust's nieuwe `time` crate kunnen zijn, met als doel een veiligere en ergonomischere API.

Wanneer je een datum naar een string converteert, ben je aan het serialiseren – je zet gegevens om naar een formaat dat gedeeld of opgeslagen kan worden. Het formaat dat je kiest (`%Y-%m-%d %H:%M:%S` in ons geval) is aan jou, en `chrono` ondersteunt veel van dergelijke patronen.

Intern worden datums vaak opgeslagen als timestamps – seconden vanaf een startpunt, zoals het Unix-tijdperk (1 januari 1970). Wanneer je een datum formatteert, bereken je de leesbare vorm vanuit deze telling, met inachtneming van tijdzones en schrikkelseconden.

## Zie Ook

- `chrono` crate documentatie: https://docs.rs/chrono/
- Rust's `time` crate documentatie: https://docs.rs/time/
- Datum formatteringssyntax: http://www.unicode.org/reports/tr35/tr35-dates.html#Date_Field_Symbol_Table
