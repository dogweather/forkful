---
title:    "Rust: Comparaison de deux dates"
keywords: ["Rust"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/fr/rust/comparing-two-dates.md"
---

{{< edit_this_page >}}

## Pourquoi

Comparer deux dates peut sembler simple au premier abord, mais en réalité cela peut être un défi de taille en programmation. Heureusement, le langage de programmation Rust offre des fonctionnalités puissantes pour faciliter cette tâche. Dans cet article, nous allons explorer comment comparer efficacement deux dates en utilisant Rust.

## Comment faire

Pour commencer, nous allons déclarer deux variables de type `Date` en utilisant la bibliothèque `chrono` de Rust :

```Rust
let date1 = chrono::NaiveDate::from_ymd(2021, 1, 1);
let date2 = chrono::NaiveDate::from_ymd(2020, 12, 31);
```

Maintenant que nous avons deux dates, nous pouvons utiliser la méthode `cmp` pour les comparer. Cette méthode retourne une valeur `Ordering` qui peut être `Less`, `Equal` ou `Greater`, selon que la première date est antérieure, égale ou postérieure à la seconde.

```Rust
let comparison = date1.cmp(&date2);
match comparison {
    Ordering::Less => println!("La première date est antérieure à la seconde."),
    Ordering::Equal => println!("Les deux dates sont égales."),
    Ordering::Greater => println!("La première date est postérieure à la seconde."),
}
```

La sortie de ce code sera "La première date est postérieure à la seconde." car 2021 est une année ultérieure à 2020.

## Plongeons plus en profondeur

Maintenant que nous savons comment comparer deux dates en utilisant Rust, il est important de comprendre la structure interne de ces dates. En Rust, les dates sont représentées en utilisant le système de calendrier proleptique grégorien, ce qui signifie qu'elles peuvent représenter des dates allant jusqu'à des milliards d'années dans le passé ou dans le futur.

De plus, la bibliothèque `chrono` fournit également des méthodes utiles pour manipuler et formater des dates, comme la méthode `format`, qui nous permet de convertir une date en une chaîne de caractères selon un format spécifique.

```Rust
let date_string = date1.format("%d/%m/%Y").to_string();
println!("La date au format jour/mois/année est : {}", date_string);
```

La sortie sera "La date au format jour/mois/année est : 01/01/2021".

## Voir aussi

- [Documentation officielle de Rust sur les méthodes de comparaison de dates](https://doc.rust-lang.org/std/primitive.struct.Date.html#method.cmp)
- [Guide de référence de la bibliothèque Chrono pour Rust](https://docs.rs/chrono/0.4.19/chrono/index.html)
- [Tutoriel sur les manipulations de dates en Rust](https://opensource.com/article/18/9/manipulating-dates-rust)

Merci d'avoir lu cet article sur la comparaison de dates en Rust ! N'hésitez pas à consulter les liens ci-dessus pour en apprendre davantage sur ce sujet.