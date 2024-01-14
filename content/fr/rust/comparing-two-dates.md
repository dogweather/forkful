---
title:                "Rust: Comparer deux dates"
programming_language: "Rust"
category:             "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fr/rust/comparing-two-dates.md"
---

{{< edit_this_page >}}

## Pourquoi

Comparer deux dates peut sembler être une tâche simple, mais cela peut s'avérer complexe en pratique. En utilisant le langage de programmation Rust, nous pouvons nous assurer que notre code est efficace et sûr. Dans cet article, nous allons découvrir comment comparer facilement deux dates en utilisant Rust.

## Comment faire

Pour commencer, nous devrons implémenter la fonction `compare_dates` qui prendra en paramètre deux dates au format `DateTime` de la bibliothèque `chrono`. Voici à quoi cela pourrait ressembler en code:

```Rust
// Importation de la bibliothèque chrono
use chrono::{DateTime, Utc};

// Implémentation de la fonction compare_dates 
fn compare_dates(date1: DateTime<Utc>, date2: DateTime<Utc>) ->&str {
    // Code pour comparer les deux dates et retourner un résultat sous forme de chaîne de caractères
    if date1 > date2 {
        "La date 1 est plus récente que la date 2"
    } else if date1 < date2 {
        "La date 2 est plus récente que la date 1"
    } else {
        "Les deux dates sont identiques"
    }
}

fn main() {
    // Définition de deux dates pour les comparer
    let date1 = Utc::now();
    let date2 = Utc::now() - Duration::days(2);

    // Appel de la fonction compare_dates et affichage du résultat
    println!("{}", compare_dates(date1, date2));
}
```

Lorsque nous exécutons ce code, nous pouvons voir que le résultat affiché est "La date 1 est plus récente que la date 2". Nous pouvons également modifier les dates pour voir différents résultats.

## Approfondissement

Maintenant que nous savons comment comparer deux dates en utilisant Rust, nous pouvons également accéder à plus d'informations sur ces dates. En utilisant la bibliothèque `chrono`, nous pouvons extraire des informations telles que l'année, le mois, le jour, etc. de chaque date et les comparer individuellement pour une analyse plus précise.

Par exemple, nous pouvons utiliser la fonction `date.year()` pour obtenir l'année de chaque date et les comparer comme ceci:

```Rust
if date1.year() > date2.year() {
    // code pour gérer le cas où l'année de la date 1 est plus grande que celle de la date 2
} else if date1.year() < date2.year() {
    // code pour gérer le cas où l'année de la date 2 est plus grande que celle de la date 1
} else {
    // code pour gérer le cas où les années sont identiques
}
```

Il existe de nombreuses autres fonctions et méthodes que nous pouvons utiliser pour comparer de manière plus approfondie deux dates en Rust, et cela dépendra des besoins spécifiques de notre projet.

## Voir aussi

- [Documentation officielle de la bibliothèque chrono pour Rust](https://docs.rs/chrono/0.4.19/chrono/)
- [Tutoriel sur l'utilisation de la bibliothèque chrono](https://stevedonovan.github.io/rust-gentle-intro/6-a-datetime.html#using-the-chrono-library)