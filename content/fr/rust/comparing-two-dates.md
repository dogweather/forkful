---
title:    "Rust: Comparaison de deux dates"
keywords: ["Rust"]
---

{{< edit_this_page >}}

# Pourquoi comparer deux dates en Rust

De nos jours, la programmation est devenue un incontournable dans presque tous les domaines et langages de programmation. Il est donc important de comprendre pourquoi il est utile de comparer deux dates en Rust.

## Comment faire

Dans Rust, il existe différentes méthodes pour comparer des dates. L'une des plus simples est d'utiliser la méthode `cmp()` qui renvoie un résultat de type `Ordering`. Voici un exemple de code pour comparer deux dates en Rust :

```Rust
use std::time::{SystemTime, UNIX_EPOCH};

// Obtenir la date actuelle
let now = SystemTime::now();

// Convertir en timestamp
let timestamp_now = now.duration_since(UNIX_EPOCH).expect("Cannot create timestamp").as_secs();

// Dates à comparer
let date1 = "2020-01-01";
let date2 = "2020-05-01";

// Convertir en timestamp
let timestamp_date1 = convert_to_timestamp(date1);
let timestamp_date2 = convert_to_timestamp(date2);

// Comparaison en utilisant la méthode cmp()
let result = timestamp_date1.cmp(&timestamp_date2);

// Afficher le résultat
if result == Ordering::Less {
  println!("La date 1 est antérieure à la date 2");
} else if result == Ordering::Greater {
  println!("La date 2 est antérieure à la date 1");
} else {
  println!("Les deux dates sont identiques");
}

// Fonction pour convertir une date en timestamp
fn convert_to_timestamp(date: &str) -> u64 {
  let date_time = format!("{} 00:00:00", date).parse::<DateTime<Utc>>().expect("Incorrect date format");
  date_time.timestamp() as u64
}
```

La sortie de ce code sera :

```
La date 1 est antérieure à la date 2
```

De plus, Rust offre également la possibilité de comparer des dates en utilisant des opérateurs de comparaison tels que `==`, `!=`, `<`, `>`, etc.

## Plongez plus profondément

Comparer deux dates peut sembler simple, mais il est important de comprendre comment les dates sont traitées par le langage. En Rust, les dates sont stockées sous forme de timestamps, qui représentent le nombre de secondes depuis le 1er janvier 1970 à minuit UTC. Cela permet une manipulation plus facile et plus précise des dates.

De plus, Rust dispose d'une bibliothèque dédiée pour gérer les dates et le temps, appelée `chrono`. Cette bibliothèque offre des fonctionnalités avancées pour manipuler et comparer des dates en Rust.

## Voir aussi

- [Documentation officielle de Rust sur les comparaisons de dates](https://doc.rust-lang.org/std/cmp/trait.Ord.html)
- [Documentation officielle de la bibliothèque `chrono`](https://docs.rs/chrono/0.3.0/chrono/)