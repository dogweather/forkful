---
aliases:
- /nl/rust/comparing-two-dates/
changelog:
- 2024-01-28, gpt-4-0125-preview, translated from English
date: 2024-01-28 21:56:30.294194-07:00
description: "Het vergelijken van twee datums betekent controleren of ze gelijk zijn,\
  \ of dat de ene voor of na de andere komt. Programmeurs gebruiken dit voor het\u2026"
lastmod: 2024-02-18 23:09:01.632990
model: gpt-4-0125-preview
summary: "Het vergelijken van twee datums betekent controleren of ze gelijk zijn,\
  \ of dat de ene voor of na de andere komt. Programmeurs gebruiken dit voor het\u2026"
title: Twee datums vergelijken
---

{{< edit_this_page >}}

## Wat & Waarom?
Het vergelijken van twee datums betekent controleren of ze gelijk zijn, of dat de ene voor of na de andere komt. Programmeurs gebruiken dit voor het sorteren van gebeurtenissen, het valideren van invoer, het afhandelen van verloopdata en het bijhouden van tijdsduren.

## Hoe te:
Rust gebruikt `chrono` om eenvoudig met datums om te gaan. Eerst moet `cargo.toml` `chrono = "0.4"` bevatten. Daarna kun je datums als volgt vergelijken:

```Rust
extern crate chrono;
use chrono::{DateTime, Utc};

fn main() {
    let datum1: DateTime<Utc> = Utc::now();
    let datum2: DateTime<Utc> = Utc::now(); // Verander dit voor verschillende resultaten

    if datum1 > datum2 {
        println!("Datum1 is later dan Datum2");
    } else if datum1 < datum2 {
        println!("Datum1 is eerder dan Datum2");
    } else {
        println!("Datum1 is gelijk aan Datum2");
    }
}
```

Voorbeelduitvoer waar `datum1` later is:

```
Datum1 is later dan Datum2
```

## Diepere Duik
In de beginjaren van Rust (jaren 2010) was datumvergelijking lastiger—geen `chrono` crate. `chrono` kwam en vereenvoudigde dingen met typen zoals `DateTime`. Voor `chrono` moesten we handmatig met tijd omgaan, gevoelig voor fouten.

Waarom `chrono`? Het abstraheert complexiteiten zoals tijdzones en schrikkeljaren, waardoor datumvergelijkingen betrouwbaar zijn. Zonder dat zou je jongleren met Unix-tijdstempels, lomp en minder leesbaar.

Er bestaan alternatieven voor `chrono`, zoals de `time` crate, maar `chrono` wordt veel gebruikt vanwege zijn eenvoudigheid en functies.

## Zie Ook
- `chrono` crate documentatie: [docs.rs/chrono](https://docs.rs/chrono/)
- Officiële Rust documentatie over datum en tijd concepten: [doc.rust-lang.org/std/time](https://doc.rust-lang.org/std/time/index.html)
- Vergelijking van `chrono` en `time` crates: [users.rust-lang.org](https://users.rust-lang.org/t/chrono-vs-time/45575)
