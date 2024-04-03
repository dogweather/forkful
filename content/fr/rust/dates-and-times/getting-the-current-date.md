---
changelog:
- 2024-02-03, gpt-4-0125-preview, translated from English
date: 2024-02-03 19:10:55.862777-07:00
description: "R\xE9cup\xE9rer la date actuelle en Rust est une t\xE2che courante pour\
  \ des op\xE9rations telles que la journalisation, les op\xE9rations bas\xE9es sur\
  \ le temps ou\u2026"
lastmod: '2024-03-13T22:44:57.493309-06:00'
model: gpt-4-0125-preview
summary: "R\xE9cup\xE9rer la date actuelle en Rust est une t\xE2che courante pour\
  \ des op\xE9rations telles que la journalisation, les op\xE9rations bas\xE9es sur\
  \ le temps ou simplement afficher la date."
title: Obtenir la date actuelle
weight: 29
---

## Quoi et Pourquoi ?

Récupérer la date actuelle en Rust est une tâche courante pour des opérations telles que la journalisation, les opérations basées sur le temps ou simplement afficher la date. Contrairement à certaines langues qui incluent la fonctionnalité de date et d'heure dans leur bibliothèque standard, Rust encourage l'utilisation d'une bibliothèque tierce robuste, chrono, pour une manipulation complète de la date et de l'heure en raison de sa fonctionnalité supérieure et de sa facilité d'utilisation.

## Comment faire :

### En utilisant la bibliothèque standard de Rust
La bibliothèque standard de Rust fournit un moyen limité mais rapide d'obtenir l'heure actuelle, bien qu'elle ne donne pas directement la date actuelle dans un format de calendrier. Voici comment vous le faites :

```rust
use std::time::{SystemTime, UNIX_EPOCH};

fn main() {
    match SystemTime::now().duration_since(UNIX_EPOCH) {
        Ok(n) => println!("Heure actuelle : {} secondes depuis l'époque Unix.", n.as_secs()),
        Err(_) => panic!("SystemTime avant l'époque Unix !"),
    }
}
```

Sortie :
```
Heure actuelle : 1615390665 secondes depuis l'époque Unix.
```

### En utilisant la bibliothèque Chrono
Pour une fonctionnalité de date et d'heure plus complète, y compris obtenir la date actuelle, vous devriez utiliser la bibliothèque `chrono`. Tout d'abord, ajoutez `chrono` à votre `Cargo.toml` :

```toml
[dependencies]
chrono = "0.4"
```

Ensuite, vous pouvez utiliser `chrono` pour obtenir la date actuelle :

```rust
extern crate chrono;
use chrono::{Local, Datelike};

fn main() {
    let now = Local::now();
    println!("Date actuelle : {}-{}-{}", now.year(), now.month(), now.day());
}
```

Sortie :
```
Date actuelle : 2023-4-20
```

La bibliothèque `chrono` simplifie le travail avec les dates et les heures, offrant une large gamme de fonctionnalités au-delà de la simple récupération de la date actuelle, y compris l'analyse, le formatage et les opérations arithmétiques sur les dates et les heures.
