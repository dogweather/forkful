---
title:                "Comparaison de deux dates"
html_title:           "Rust: Comparaison de deux dates"
simple_title:         "Comparaison de deux dates"
programming_language: "Rust"
category:             "Rust"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fr/rust/comparing-two-dates.md"
---

{{< edit_this_page >}}

Que sont les comparaisons de dates et pourquoi les programmeurs les utilisent-ils?

Les comparaisons de dates sont une méthode permettant de vérifier si deux dates sont identiques ou si l'une est avant ou après l'autre. Les programmeurs les utilisent souvent pour trier des données chronologiquement ou pour vérifier si une date est incluse dans une période spécifique.

Comment faire:

```Rust
let date1 = chrono::NaiveDate::from_ymd(2021, 10, 15);
let date2 = chrono::NaiveDate::from_ymd(2021, 10, 20);

// Comparaison de deux dates pour vérifier si elles sont identiques
if date1 == date2 {
    println!("Les dates sont identiques!");
}

// Comparaison pour vérifier si une date est avant l'autre
if date1 < date2 {
    println!("La première date est avant la deuxième !");
}

// Comparaison pour vérifier si une date est après l'autre
if date1 > date2 {
    println!("La première date est après la deuxième !");
}
```

Ce plongeon en profondeur vous fournira des informations supplémentaires sur les comparaisons de dates:

Histoire:

Les comparaisons de dates sont une fonctionnalité courante dans la programmation depuis de nombreuses années. Les langages de programmation tels que C et Java ont des opérateurs spécifiques pour les comparaisons de dates, tandis que d'autres, comme Python et Rust, utilisent des méthodes ou des fonctions dédiées.

Alternatives:

Il existe plusieurs façons de comparer des dates en Rust. En plus des opérateurs de comparaison, comme dans les exemples précédents, vous pouvez également utiliser les méthodes ```eq```, ```lt```, ```gt```, ```le``` et ```ge``` pour vérifier l'égalité, l'infériorité, la supériorité, l'infériorité ou la supériorité égale de deux dates.

Détails d'implémentation:

En interne, la bibliothèque de dates et d'heures de Rust, ```chrono```, convertit les dates en nombres entiers et les compare ensuite en utilisant les opérateurs spécifiques de Rust. Elle prend également en compte les différents formats de dates possibles, tels que le format ISO 8601 ou le format ISO 3339.

Voir aussi:

Documentation pour la bibliothèque de dates et d'heures de Rust: https://docs.rs/chrono/.

Documentation officielle de Rust pour les opérateurs de comparaison: https://doc.rust-lang.org/std/cmp/.