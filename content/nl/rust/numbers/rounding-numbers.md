---
changelog:
- 2024-01-28, gpt-4-0125-preview, translated from English
date: 2024-01-28 22:06:46.167999-07:00
description: "Getallen afronden betekent ze aanpassen naar het dichtstbijzijnde hele\
  \ getal of een fractie met een bepaalde precisie. Programmeurs ronden getallen af\
  \ om\u2026"
lastmod: '2024-02-25T18:49:47.933167-07:00'
model: gpt-4-0125-preview
summary: "Getallen afronden betekent ze aanpassen naar het dichtstbijzijnde hele getal\
  \ of een fractie met een bepaalde precisie. Programmeurs ronden getallen af om\u2026"
title: Afronden van getallen
---

{{< edit_this_page >}}

## Wat & Waarom?
Getallen afronden betekent ze aanpassen naar het dichtstbijzijnde hele getal of een fractie met een bepaalde precisie. Programmeurs ronden getallen af om waarden te vereenvoudigen voor menselijke leesbaarheid, om aan specificatievereisten te voldoen of om de computationele overhead in zwevendekommabewerkingen te verminderen.

## Hoe:
Rust maakt afronden een fluitje van een cent. Bekijk deze methoden voor `f32` of `f64` types:

```rust
fn main() {
    let num = 2.34567;

    // Afronden naar het dichtstbijzijnde hele getal
    let round = num.round();
    println!("Afronden: {}", round); // Afronden: 2

    // Vloer - grootste geheel getal kleiner dan of gelijk aan nummer
    let floor = num.floor();
    println!("Vloer: {}", floor); // Vloer: 2

    // Plafond - kleinste geheel getal groter dan of gelijk aan nummer
    let ceil = num.ceil();
    println!("Plafond: {}", ceil); // Plafond: 3

    // Trunceren - geheel getaldeel zonder fractionele cijfers
    let trunc = num.trunc();
    println!("Trunceren: {}", trunc); // Trunceren: 2

    // Naar het dichtstbijzijnde veelvoud van een macht van tien
    let multiple_of_ten = (num * 100.0).round() / 100.0;
    println!("Afgerond op 2 decimalen: {}", multiple_of_ten); // Afgerond op 2 decimalen: 2.35
}
```

## Diepere Duik
Historisch gezien is afronden cruciaal geweest voor het passend maken van oneindige decimalen of irrationele getallen in beperkte digitale ruimtes—een must voor oude computers met schaarse geheugen. Denk aan een abacus, maar minder handig, meer wiskunde.

Alternatieven voor de native Rust-methoden zijn onder andere:
1. `format!` macro voor stringformatting die standaard afrondt.
2. Externe crates voor gespecialiseerde wiskundige taken, zoals de `round` crate met meer gedetailleerde controle.

Onder de motorkap voldoen de afrondingsoperaties van Rust aan de IEEE-normen—technisch jargon voor "het rondt af zoals je wiskundeleraar wil." Bovendien kunnen sommige getallen, vanwege binaire representaties, zoals 0.1, niet traditioneel worden afgerond, vanwege hun oneindige representatie in binaire vorm.

## Zie Ook
- Rust doc over methoden van primitieve types: https://doc.rust-lang.org/std/primitive.f64.html
- IEEE Standaard voor Zwevendekomma Rekenkunde (IEEE 754): https://ieeexplore.ieee.org/document/4610935
- "round" crate voor complexere afronding: https://crates.io/crates/round
