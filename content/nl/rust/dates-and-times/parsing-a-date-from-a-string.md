---
title:                "Een datum uit een string parsen"
aliases: - /nl/rust/parsing-a-date-from-a-string.md
date:                  2024-01-28T22:04:09.332106-07:00
model:                 gpt-4-0125-preview
simple_title:         "Een datum uit een string parsen"

tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/nl/rust/parsing-a-date-from-a-string.md"
changelog:
  - 2024-01-28, gpt-4-0125-preview, translated from English
---

{{< edit_this_page >}}

## Wat & Waarom?
Het parsen van een datum uit een string betekent het converteren van tekst naar een datumformaat dat je code kan begrijpen. We doen dit omdat datums vaak als strings van gebruikersinvoer of externe gegevensbronnen komen en we ze in een gestructureerde vorm nodig hebben voor berekening en opslag.

## Hoe:
Om datums in Rust te parsen, gebruiken we de `chrono` crate, een go-to bibliotheek voor datum en tijd.

Voeg eerst `chrono` toe aan je `Cargo.toml`:

```toml
[dependencies]
chrono = "0.4"
```

Vervolgens, hier is een simpel voorbeeld van het parsen van een ISO 8601 datum:

```rust
extern crate chrono;
use chrono::prelude::*;

fn main() {
    let date_str = "2023-04-05";
    let parsed_date = date_str.parse::<NaiveDate>().unwrap();

    println!("Geparste datum is: {}", parsed_date);
}

```
Output:
```
Geparste datum is: 2023-04-05
```

## Diepere Duik
`chrono` is Rusts keuze voor datum- en tijdsparsing, zo ongeveer sinds de oprichting van Rust. Voor `chrono` had Rust een basisbibliotheek voor tijd, maar het miste functies. `chrono` vulde die leemte.

Als alternatieven heb je de `time` crate, maar `chrono` wint in populariteit en functieset. Op implementatieniveau gaat het parsen van een datumstring over het specificeren van het formaat en het afhandelen van de mogelijkheid van falen—daarom gebruikten we `unwrap()`, wat prima is in voorbeelden, maar gebruik `match` of `unwrap_or_else` in echte code om fouten sierlijk af te handelen.

Historisch gezien hebben programmeertalen moeite gehad met datum en tijd. Het is complex vanwege schrikkeljaren, tijdzones en veranderingen in zomertijd. Dat is waarom crates zoals `chrono` waardevol zijn—ze handelen de eigenaardigheden voor ons af.

## Zie Ook
- Officiële `chrono` crate documentatie: https://docs.rs/chrono/
- Rust API richtlijnen over foutafhandeling: https://rust-lang.github.io/api-guidelines/error.html
- Een diepgaande kijk op de geschiedenis van Rusts tijdsbibliotheek: https://www.reddit.com/r/rust/comments/2z54zb/history_of_rusts_time_library/
