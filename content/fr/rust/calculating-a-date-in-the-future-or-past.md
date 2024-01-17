---
title:                "Calculer une date dans le futur ou le passé"
html_title:           "Rust: Calculer une date dans le futur ou le passé"
simple_title:         "Calculer une date dans le futur ou le passé"
programming_language: "Rust"
category:             "Rust"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fr/rust/calculating-a-date-in-the-future-or-past.md"
---

{{< edit_this_page >}}

## Qu'est-ce que c'est et pourquoi?

Calculer une date dans le futur ou le passé consiste à utiliser un algorithme pour trouver la date qui correspond à un certain nombre de jours avant ou après une date donnée. Les programmeurs utilisent souvent cette technique pour des tâches telles que la planification de rendez-vous, le calcul de délais ou le traitement de données chronologiques.

## Comment faire:

Voici un exemple de code en Rust qui utilise la bibliothèque standard pour calculer une date dans le futur:

```Rust
// Importer la bibliothèque "chrono"
use chrono::{NaiveDate, Duration};

// Définir une date de départ
let start_date = NaiveDate::from_ymd(2021, 11, 24);

// Ajouter 10 jours à la date de départ
let future_date = start_date + Duration::days(10);

// Afficher le résultat
println!("La date dans 10 jours sera: {}", future_date);

// Output: La date dans 10 jours sera: 2021-12-04
```

Vous pouvez également utiliser la bibliothèque "time" pour calculer une date dans le passé:

```Rust
// Importer la bibliothèque "time"
use time::Date;

// Définir une date de fin
let end_date = Date::from_utc(time::OffsetDateTime::from_unix_timestamp(1624284000), time::Utc);

// Soustraire 7 jours à la date de fin
let past_date = end_date - time::Duration::days(7);

// Afficher le résultat
println!("La date il y a 7 jours était: {}", past_date);

// Output: La date il y a 7 jours était: 2021-06-15
```

## Plongez plus en profondeur:

Calculer des dates dans le futur ou le passé est une tâche courante dans la programmation depuis de nombreuses années. Avant l'essor de bibliothèques spécialisées telles que "chrono" et "time", les programmeurs devaient souvent écrire leur propre code pour gérer ces calculs.

Parmi les alternatives à ces bibliothèques, vous pouvez trouver des packages tiers tels que "date" et "datetime" qui proposent également des fonctionnalités de calcul de dates.

En termes d'implémentation, la plupart de ces bibliothèques utilisent des algorithmes de base tels que l'addition et la soustraction de jours pour calculer les dates futures ou passées. Cependant, elles peuvent également prendre en compte des facteurs tels que les années bissextiles et les fuseaux horaires pour une précision maximale.

## Voir aussi:

- [Documentation de la bibliothèque "chrono"](https://docs.rs/chrono/0.4.19/chrono/)
- [Documentation de la bibliothèque "time"](https://docs.rs/time/0.2.25/time/)
- [Autres bibliothèques pour calculer des dates en Rust](https://crates.io/search?q=date)