---
title:                "Analyser une date à partir d'une chaîne"
html_title:           "Clojure: Analyser une date à partir d'une chaîne"
simple_title:         "Analyser une date à partir d'une chaîne"
programming_language: "Rust"
category:             "Rust"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fr/rust/parsing-a-date-from-a-string.md"
---

{{< edit_this_page >}}

## Qu'est-ce que c'est et pourquoi?

Parsing une date d'une chaîne signifie conversion d'une string représentant une date dans un format spécifique vers un type de données de date. Les programmeurs en ont besoin pour manipuler et utiliser des dates provenant de différentes sources de données.

## Comment faire :

Voici comment convertir une string en une date en utilisant la bibliothèque `Chrono`, un paquet Rust pour traiter les dates et les heures. Assurez-vous de l'ajouter comme une dépendance dans `Cargo.toml`.

```rust
[dependencies]
chrono = "0.4"
```

Vous pouvez maintenant l'importer et l'utiliser comme ça :

```rust
extern crate chrono;

use chrono::NaiveDate;

fn main() {
    let date_str = "2022-01-01";
    let date = NaiveDate::parse_from_str(date_str, "%Y-%m-%d").expect("Unable to parse date");
    println!("{}", date);
}
```

Si vous exécutez le code ci-dessus, vous verrez l'output suivant :

```rust
2022-01-01
```

## Plongée profonde:

La manipulation des dates a été un défi pour les programmeurs depuis les débuts de l'informatique. Chaque langue a ses propres caractéristiques uniques liées au parsing des dates.

En Rust, une alternative à `Chrono` est la bibliothèque `time`. C'est plus bas niveau et plus léger que `Chrono`, mais nécessite plus de code pour accomplir des tâches similaires.

L'implémentation du parsing de chaînes en dates repose sur le principe simple de l'analyse syntaxique. Pour accomplir cette tâche, Rust effectue une correspondance de modèles sur la string de date en utilisant le format spécifié.

## Voir Aussi :

Pour plus d'informations sur la manipulation de date et heure en Rust, consultez ces ressources :

1. La documentation Rust sur les dates et heures : https://doc.rust-lang.org/book/ch10-04-syntax.html
2. La documentation `chrono` : https://docs.rs/chrono/0.4.19/chrono/
3. La documentation `time` : https://docs.rs/time/0.3.5/time/