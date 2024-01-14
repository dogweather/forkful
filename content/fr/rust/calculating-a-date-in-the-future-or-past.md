---
title:                "Rust: Calculer une date dans le futur ou le passé."
programming_language: "Rust"
category:             "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fr/rust/calculating-a-date-in-the-future-or-past.md"
---

{{< edit_this_page >}}

## Pourquoi

Calendrier mathématique pourrait sembler être un sujet inutile, mais en réalité, c'est une compétence utile pour de nombreux programmes et services, notamment ceux qui impliquent des rappels, des deadlines ou des plannings. Apprendre à calculer une date dans le futur ou dans le passé en Rust peut donc être un moyen efficace d'améliorer vos compétences en programmation.

## Comment faire

Pour calculer une date dans le futur ou dans le passé en Rust, vous pouvez utiliser la bibliothèque "chrono". Tout d'abord, importez la bibliothèque en utilisant "extern crate" dans votre code :

```Rust
extern crate chrono;
```

Ensuite, vous pouvez utiliser la méthode "parse_from_str" et lui donner une chaîne de caractères qui représente une date au format ISO 8601 :

```Rust
use chrono::prelude::*;
let date = NaiveDate::parse_from_str("2021-10-15", "%Y-%m-%d").unwrap();
```

Maintenant, vous pouvez utiliser la méthode "checked_add" ou "checked_sub" pour ajouter ou soustraire une certaine quantité de temps à la date d'origine :

```Rust
// Pour ajouter 3 jours à la date originale :
let future = date.checked_add_signed(Duration::days(3)).unwrap();

// Pour soustraire 6 mois à la date originale :
let past = date.checked_sub_signed(Duration::days(6 * 30)).unwrap();
```

## Plongée profonde

Maintenant que vous savez comment utiliser la bibliothèque "chrono" pour calculer une date dans le futur ou dans le passé, il est important de comprendre exactement ce qui se passe en coulisses. En utilisant la méthode "parse_from_str", vous créez un objet "NaiveDate" qui représente une date sans prendre en compte les fuseaux horaires ou les calendriers. Cela permet d'effectuer facilement des calculs sans avoir à se soucier de ces éléments.

Ensuite, la méthode "checked_add" ou "checked_sub" utilise un objet "Duration" pour représenter la quantité de temps à ajouter ou à soustraire. Ce type de données est utilisé pour garantir que les opérations de calendrier sont toujours valides et ne produisent pas des dates impossibles (comme le 30 février).

## Voir aussi

- [Documentation officielle de la bibliothèque "chrono" pour Rust](https://docs.rs/chrono/latest/chrono/)
- [Tutoriel sur la gestion des dates en Rust](https://opensource.com/article/19/4/working-dates-rust)
- [Article sur l'utilisation de la bibliothèque "chrono" pour manipuler les fuseaux horaires en Rust](https://bytes.zone/posts/dates-and-times-rust/)