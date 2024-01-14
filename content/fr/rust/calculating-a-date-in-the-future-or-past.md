---
title:                "Rust: Calculer une date dans le futur ou le passé"
simple_title:         "Calculer une date dans le futur ou le passé"
programming_language: "Rust"
category:             "Rust"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fr/rust/calculating-a-date-in-the-future-or-past.md"
---

{{< edit_this_page >}}

## Pourquoi

Lorsque vous construisez des applications ou des sites Web, il y a souvent des scénarios où vous devez calculer une date dans le futur ou dans le passé. Cela peut être pour afficher une date d'expiration, planifier des événements ou simplement pour effectuer des opérations de temps. Dans cet article, nous allons explorer comment réaliser cela en utilisant le langage de programmation Rust.

## Comment faire

La première étape pour calculer une date dans le futur ou dans le passé est d'utiliser le type de données `chrono::DateTime` de Rust. Il s'agit d'une structure qui stocke une date et une heure précises. Ensuite, vous pouvez utiliser des méthodes telles que `add`et `subtract` pour ajouter ou soustraire une certaine durée de la date.

Voici un exemple de code montrant comment calculer une date dans le futur de 30 jours à partir de la date actuelle :

```Rust
use chrono::DateTime;
use chrono::Duration;
use chrono::Utc;
    
let now: DateTime<Utc> = Utc::now();
let future_date: DateTime<Utc> = now.add(Duration::days(30));
```
Le résultat sera une date dans le futur avec une différence de 30 jours par rapport à la date actuelle.

De même, voici un exemple de code pour calculer une date dans le passé de 2 semaines à partir de la date actuelle :

```Rust
use chrono::DateTime;
use chrono::Duration;
use chrono::Utc;
    
let now: DateTime<Utc> = Utc::now();
let past_date: DateTime<Utc> = now.subtract(Duration::weeks(2));
```

Le résultat sera une date dans le passé avec une différence de 2 semaines par rapport à la date actuelle.

## Plongée profonde

Bien que les exemples ci-dessus montrent comment calculer une date dans le futur ou dans le passé en utilisant des durées spécifiques, il est également possible de le faire en utilisant des unités de temps plus précises telles que les secondes, les minutes ou les heures. Vous pouvez également combiner plusieurs unités de temps pour obtenir une durée plus précise. Cela peut être utile pour des tâches telles que le suivi du temps ou le calcul de délais de livraison.

Vous pouvez également utiliser des méthodes telles que `with_UTC_offset` pour spécifier un décalage horaire spécifique, ou `with_timezone` pour définir un fuseau horaire différent de celui par défaut.

## Voir aussi

- [Document officiel sur le type `chrono::DateTime`](https://docs.rs/chrono/0.4.13/chrono/struct.DateTime.html)
- [Guide de la bibliothèque `chrono` pour le calcul du temps](https://docs.rs/chrono/0.4.13/chrono/trait.Sub.html)
- [Exemples de code pour calculer des dates en utilisant Rust](https://github.com/chronotope/chrono/blob/master/examples/examples.rs)