---
changelog:
- 2024-01-28, gpt-4-0125-preview, translated from English
date: 2024-01-28 21:55:40.402980-07:00
description: 'Hoe te: Rust heeft de `chrono` crate voor al je datum- en tijdsbehoeften.
  Hier is hoe je een datum kunt optellen of aftrekken.'
lastmod: '2024-03-13T22:44:50.607802-06:00'
model: gpt-4-0125-preview
summary: Rust heeft de `chrono` crate voor al je datum- en tijdsbehoeften.
title: Een datum in de toekomst of het verleden berekenen
weight: 26
---

## Hoe te:
Rust heeft de `chrono` crate voor al je datum- en tijdsbehoeften. Hier is hoe je een datum kunt optellen of aftrekken:

```rust
use chrono::{DateTime, Duration, Utc};

fn main() {
    let now = Utc::now();
    println!("Huidige UTC-tijd: {}", now);

    let twee_weken = Duration::weeks(2);
    let toekomstige_datum = now + twee_weken;
    println!("UTC over twee weken: {}", toekomstige_datum);

    let dertig_dagen_geleden = Duration::days(-30);
    let verleden_datum = now + dertig_dagen_geleden;
    println!("UTC 30 dagen geleden: {}", verleden_datum);
}
```

Voorbeelduitvoer:

```
Huidige UTC-tijd: 2023-04-01T12:00:00Z
UTC over twee weken: 2023-04-15T12:00:00Z
UTC 30 dagen geleden: 2023-03-02T12:00:00Z
```

## Uitdieping
Traditioneel is het manipuleren van datum en tijd een lastige taak geweest. Verschillende systemen en programmeertalen gaan er op verschillende manieren mee om. De standaardbibliotheek van Rust biedt basisfunctionaliteit, maar de `chrono` crate is de gangbare oplossing.

Alternatieven? Zeker, je zou handmatig data kunnen berekenen door alles om te zetten naar timestamps, met de getallen te manipuleren en vervolgens terug te converteren. Of, je zou tijdsspecifieke bibliotheken in andere talen kunnen gebruiken - Python heeft `datetime`, JavaScript heeft `Date`, enzovoort.

De `chrono` crate in Rust geeft je tijdzonebewuste typen zoals `DateTime`, en duur zoals hierboven gezien. Het handelt alle lastige zaken af zoals schrikkeljaren en zomertijd zodat jij dat niet hoeft te doen. Het doet ook datum parsing en formatting, waardoor het een uitgebreide oplossing is.

## Zie Ook
- De `chrono` crate: https://crates.io/crates/chrono
- Rust's tijddocumentatie: https://doc.rust-lang.org/std/time/index.html
- Rust Datum en Tijd hoofdstuk in het boek "The Rust Programming Language": https://doc.rust-lang.org/book/ch10-02-traits.html (zoek naar DateTime-gerelateerde secties)
