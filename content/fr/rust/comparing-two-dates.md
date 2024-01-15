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

# Pourquoi comparer deux dates en Rust ?

Comparer des dates peut sembler une tâche simple à première vue, mais cela peut vite devenir une source de frustration si vous ne savez pas exactement comment le faire en utilisant le langage Rust. Dans cet article, nous allons expliquer pourquoi il est utile de comparer deux dates et vous montrer comment le faire en utilisant Rust. Nous plongerons également un peu plus profondément dans les détails pour mieux comprendre les différents aspects de la comparaison de dates en Rust.

## Comment procéder

```Rust
use chrono::{Datelike, naieve::NaiveDate};

let date1 = NaiveDate::from_ymd(2021, 12, 10);
let date2 = NaiveDate::from_ymd(2022, 1, 1);

if date1 < date2 {
    println!("La date 1 est antérieure à la date 2.");
} else if date1 > date2 {
    println!("La date 2 est antérieure à la date 1.");
} else {
    println!("Les deux dates sont identiques.");
}

// Output: La date 1 est antérieure à la date 2.
```

La première étape pour comparer deux dates en Rust est d'importer le module `chrono` qui fournit des outils pour manipuler les dates et les heures. Ensuite, nous pouvons créer deux objets de type `NaiveDate` en utilisant la méthode `from_ymd` et en passant les éléments de la date (année, mois et jour) en tant que paramètres.

Une fois que nous avons nos deux dates, nous pouvons utiliser les opérateurs de comparaison (`<`, `>`, `==`) pour les comparer. Dans notre exemple, nous vérifions si la date 1 est antérieure à la date 2, si elle est postérieure ou si les deux dates sont identiques. Enfin, nous imprimons le résultat en utilisant la fonction `println!` avec un message personnalisé.

Il est également possible de comparer des dates avec des heures en utilisant le type `NaiveDateTime` et en utilisant les mêmes opérateurs de comparaison.

## Plonger plus en profondeur

Maintenant que nous savons comment comparer des dates en Rust, il est important de comprendre certains détails plus techniques. Tout d'abord, il est important de noter que la comparaison de dates est basée sur la logique des calendriers, ce qui signifie que les années bissextiles et les différents nombres de jours dans chaque mois sont pris en compte.

De plus, il existe plusieurs façons de représenter les dates en Rust, notamment en utilisant le type `DateTime` du module `chrono`, qui prend en charge les fuseaux horaires. La comparaison de dates avec des fuseaux horaires peut être plus complexe car cela implique de tenir compte des différentes règles de décalage horaire.

Il est également possible de comparer des dates en utilisant des bibliothèques externes telles que `date-comparer` ou `paris`, qui offrent des fonctionnalités supplémentaires et une syntaxe plus concise pour comparer des dates.

## Voir aussi

- [Documentation officielle de Rust sur la manipulation des dates](https://doc.rust-lang.org/std/time/index.html)
- [Documentation de la bibliothèque `chrono`](https://docs.rs/chrono/0.4.19/chrono/)
- [Comparaison de dates avec des bibliothèques externes](https://libs.rs/comparisons/date-and-time/)