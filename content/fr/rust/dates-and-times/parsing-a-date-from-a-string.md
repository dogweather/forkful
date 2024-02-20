---
changelog:
- 2024-02-03, gpt-4-0125-preview, translated from English
date: 2024-02-03 19:15:14.230399-07:00
description: "L'analyse d'une date \xE0 partir d'une cha\xEEne de caract\xE8res est\
  \ une t\xE2che courante lorsqu'il s'agit de g\xE9rer les entr\xE9es utilisateur\
  \ ou de lire des donn\xE9es\u2026"
lastmod: 2024-02-19 22:05:16.319637
model: gpt-4-0125-preview
summary: "L'analyse d'une date \xE0 partir d'une cha\xEEne de caract\xE8res est une\
  \ t\xE2che courante lorsqu'il s'agit de g\xE9rer les entr\xE9es utilisateur ou de\
  \ lire des donn\xE9es\u2026"
title: "Analyser une date depuis une cha\xEEne de caract\xE8res"
---

{{< edit_this_page >}}

## Quoi & Pourquoi ?

L'analyse d'une date à partir d'une chaîne de caractères est une tâche courante lorsqu'il s'agit de gérer les entrées utilisateur ou de lire des données depuis des fichiers, ce qui implique de convertir les données en chaîne dans un format de date reconnu par le langage de programmation. En Rust, cela est essentiel pour les opérations sur les dates, comme les comparaisons, l'arithmétique ou le formatage, et cela renforce la validation et l'intégrité des données dans les applications.

## Comment faire :

### Utiliser la bibliothèque standard de Rust (`crate chrono`)
La bibliothèque standard de Rust n'inclut pas directement l'analyse de date, mais la `crate chrono`, largement utilisée, est une solution robuste pour la manipulation de dates et d'heures. Tout d'abord, ajoutez `chrono` à votre `Cargo.toml` :

```toml
[dependencies]
chrono = "0.4"
```

Ensuite, utilisez `chrono` pour analyser une chaîne de date en un objet `NaiveDate` :

```rust
extern crate chrono;
use chrono::NaiveDate;

fn main() {
    let date_str = "2023-04-01";
    let date = NaiveDate::parse_from_str(date_str, "%Y-%m-%d")
        .expect("Échec de l'analyse de la date");

    println!("Date analysée : {}", date);
}

// Sortie exemple :
// Date analysée : 2023-04-01
```

### Utiliser la gestion avancée des dates et heures en Rust (`crate time`)
Pour une gestion plus avancée des dates et heures, y compris une analyse plus ergonomique, considérez la `crate time`. Tout d'abord, incluez-la dans votre `Cargo.toml` :

```toml
[dependencies]
time = "0.3"
```

Ensuite, analysez une chaîne de date en utilisant le type `Date` et `PrimitiveDateTime` :

```rust
use time::{Date, PrimitiveDateTime, macros::datetime};

fn main() {
    let date_str = "2023-04-01 12:34:56";
    let parsed_date = PrimitiveDateTime::parse(
        date_str, 
        &datetime!("%Y-%m-%d %H:%M:%S")
    ).expect("Échec de l'analyse de la date et de l'heure");

    println!("Date et heure analysées : {}", parsed_date);
}

// Sortie exemple :
// Date et heure analysées : 2023-04-01 12:34:56
```

Ces deux exemples montrent comment Rust, avec l'aide de crates tierces, facilite l'analyse de chaînes de dates en objets de date manipulables, en faisant un outil puissant pour le développement logiciel impliquant des données temporelles.
