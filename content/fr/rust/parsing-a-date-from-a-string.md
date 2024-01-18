---
title:                "Analyser une date à partir d'une chaîne de caractères"
html_title:           "Rust: Analyser une date à partir d'une chaîne de caractères"
simple_title:         "Analyser une date à partir d'une chaîne de caractères"
programming_language: "Rust"
category:             "Rust"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fr/rust/parsing-a-date-from-a-string.md"
---

{{< edit_this_page >}}

## Quoi et Pourquoi?

Analyser une date à partir d'une chaîne de caractères est le processus de conversion d'une date écrite sous forme de texte en un format que les ordinateurs peuvent comprendre et manipuler. Les programmeurs le font afin de pouvoir utiliser des dates dans leur code, telles que les comparer ou les afficher à l'utilisateur.

## Comment faire:

```Rust
use chrono::prelude::*;

// Convertir une chaîne de caractères en une date:
let date = NaiveDate::parse_from_str("15/03/2021", "%d/%m/%Y");

// Afficher la date dans un format spécifique:
println!("{}", date.unwrap().format("%A, %-d %B %Y"));

// Output: Lundi, 15 Mars 2021
```

## Plongée en profondeur:

Avant l'utilisation de bibliothèques telles que `chrono` en Rust, les programmeurs devaient écrire leur propre code pour analyser les dates à partir de chaînes de caractères. D'autres alternatives existent également, comme l'utilisation de fonctions de la bibliothèque standard de Rust telles que `DateTime::parse_from_rfc3339`.

Les bibliothèques de parsing de dates peuvent être sujettes à des erreurs, car les formats de dates peuvent varier en fonction des régions et des langues. Par conséquent, il est important d'être conscient des paramètres que vous utilisez pour analyser une date et de vérifier la validité du résultat.

## À voir aussi:

- [La documentation de la bibliothèque `chrono`](https://docs.rs/chrono/latest/chrono/)
- [La documentation de la bibliothèque standard de Rust sur le parsing de dates](https://doc.rust-lang.org/std/datetime/struct.DateTime.html#method.parse_from_rfc3339)