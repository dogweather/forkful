---
date: 2024-01-20 17:32:12.152076-07:00
description: "Comment faire : Avant, on manipulait les dates en C avec `time.h`, qui\
  \ \xE9tait d\xE9licat. Rust offre la crate `chrono` pour une meilleure gestion des\
  \ dates et\u2026"
lastmod: '2024-04-05T22:51:11.584167-06:00'
model: gpt-4-1106-preview
summary: "Avant, on manipulait les dates en C avec `time.h`, qui \xE9tait d\xE9licat."
title: "Calcul d'une date future ou pass\xE9e"
weight: 26
---

## Comment faire :
```Rust
use chrono::{Duration, Utc};

fn main() {
    let today = Utc::now();
    let five_days_later = today + Duration::days(5);
    let five_days_before = today - Duration::days(5);

    println!("Aujourd'hui: {}", today.format("%d-%m-%Y %H:%M:%S"));
    println!("Dans cinq jours: {}", five_days_later.format("%d-%m-%Y %H:%M:%S"));
    println!("Il y a cinq jours: {}", five_days_before.format("%d-%m-%Y %H:%M:%S"));
}
```
Résultats:
```
Aujourd'hui: 02-04-2023 14:20:15
Dans cinq jours: 07-04-2023 14:20:15
Il y a cinq jours: 28-03-2023 14:20:15
```

## Plongée en profondeur
Avant, on manipulait les dates en C avec `time.h`, qui était délicat. Rust offre la crate `chrono` pour une meilleure gestion des dates et du temps. Alternativement, on peut utiliser la bibliothèque standard, mais elle est plus limitée. `chrono` gère les fuseaux horaires, précisions sub-secondaires et facilite les calculs.

Les détails d’implémentation sont importants car les dates gèrent les années bissextiles, les fuseaux horaires et d'autres subtilités. `chrono` abstrait cela, permettant des manipulations fiables et moins d'erreurs.

## Voir aussi
- Documentation de `chrono`: https://docs.rs/chrono/
- Rust CookBook, manipulation des dates et temps: https://rust-lang-nursery.github.io/rust-cookbook/datetime.html
- Rust by Example, traitant du temps: https://doc.rust-lang.org/rust-by-example/std_misc/datetime.html
