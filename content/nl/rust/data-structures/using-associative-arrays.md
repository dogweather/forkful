---
changelog:
- 2024-01-30, gpt-4-0125-preview, translated from English
date: 2024-01-30 19:12:53.579865-07:00
description: "Associatieve arrays, of wat Rustaceans \"hash maps\" noemen, zijn collecties\
  \ die gegevens opslaan in sleutel-waardeparen. Programmeurs gebruiken ze voor\u2026"
lastmod: '2024-03-13T22:44:50.587006-06:00'
model: gpt-4-0125-preview
summary: Associatieve arrays, of wat Rustaceans "hash maps" noemen, zijn collecties
  die gegevens opslaan in sleutel-waardeparen.
title: Gebruik van associatieve arrays
weight: 15
---

## Wat & Waarom?

Associatieve arrays, of wat Rustaceans "hash maps" noemen, zijn collecties die gegevens opslaan in sleutel-waardeparen. Programmeurs gebruiken ze voor snelle gegevensopzoeking, waardoor efficiënte gegevensmanipulatie op basis van unieke sleutels mogelijk is.

## Hoe te:

In Rust biedt het type `HashMap` uit de module `std::collections` de functionaliteit van associatieve arrays. Hier is hoe je ermee kunt werken:

```Rust
use std::collections::HashMap;

fn main() {
    // Een nieuwe HashMap aanmaken
    let mut scores = HashMap::new();

    // Waarden invoegen
    scores.insert(String::from("Blauw"), 10);
    scores.insert(String::from("Geel"), 50);

    // Waarden toegang verkrijgen
    let team_naam = String::from("Blauw");
    if let Some(score) = scores.get(&team_naam) {
        println!("Score voor team Blauw: {}", score); // Uitvoer: Score voor team Blauw: 10
    }

    // Een waarde bijwerken
    scores.entry(String::from("Blauw")).and_modify(|e| *e += 5);

    // Itereren over sleutel-waardeparen
    for (sleutel, waarde) in &scores {
        println!("{}: {}", sleutel, waarde); // Uitvoer: Blauw: 15, Geel: 50
    }
}
```

## Diepere Duik

De `HashMap` in Rust gebruikt een hashfunctie om sleutels aan waarden te koppelen, wat snelle gegevensopvraging mogelijk maakt. Echter, deze efficiëntie komt met een kostenpost: hash maps behouden de volgorde van hun elementen niet. Dit staat in contrast met andere implementaties van associatieve arrays, zoals die in Python (`dict`) of Ruby, die in recente versies de invoegorde als een functie behouden. Voor gebruikssituaties waar de volgorde van sleutel-waardeparen belangrijk is, kunnen Rust-ontwikkelaars overwegen om de `BTreeMap` uit de module `std::collections` te gebruiken, welke de orde behoudt maar mogelijk langzamer is in invoegen en opvragen vergeleken met `HashMap`. Uiteindelijk hangt de keuze tussen `HashMap` en `BTreeMap` af van specifieke eisen rond ordening en prestatie.
