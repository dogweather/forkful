---
title:                "Rust: Obtenir la date actuelle"
programming_language: "Rust"
category:             "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fr/rust/getting-the-current-date.md"
---

{{< edit_this_page >}}

## Pourquoi

Avez-vous déjà eu besoin de connaître la date actuelle dans vos programmes Rust? Peut-être que vous voulez afficher la date à l'utilisateur ou peut-être que vous voulez enregistrer la date dans une base de données. Dans cet article, nous allons explorer différentes façons d'obtenir la date actuelle dans vos projets Rust.

## Comment faire

Il existe différentes façons d'obtenir la date actuelle dans Rust, mais la méthode la plus simple consiste à utiliser la fonction "Local::now ()" de la bibliothèque "chrono". Voici un exemple de code utilisant cette fonction:

```Rust
use chrono::{Local, DateTime};

// Obtenez la date et l'heure actuelles
let now: DateTime<Local> = Local::now();

// Afficher la date et l'heure au format ISO 8601
println!("{}", now);
```

La sortie de ce code sera quelque chose comme "2021-04-07T14:30:00+02:00". Selon vos besoins, vous pouvez formater la date et l'heure dans un autre format en utilisant les méthodes de la structure DateTime.

Si vous souhaitez obtenir uniquement la date sans l'heure, vous pouvez utiliser la méthode "date()" de la structure DateTime:

```Rust
// Obtenez la date actuelle
let today = now.date();

// Afficher la date au format ISO 8601
println!("{}", today);
```

## Plongée en profondeur

La bibliothèque "chrono" utilise en interne la fonction "time::now()" du langage C pour obtenir la date actuelle. Cela signifie que l'heure et la date peuvent être affectées par le fuseau horaire de votre système d'exploitation.

Si vous souhaitez obtenir la date et l'heure en utilisant un fuseau horaire spécifique, vous pouvez utiliser la fonction "with_timezone()" de la structure DateTime:

```Rust
use chrono::{Local, DateTime, FixedOffset};

// Obtenez la date et l'heure actuelles dans le fuseau horaire GMT+1
let now: DateTime<FixedOffset> = Local::now().with_timezone(&FixedOffset::east(1 * 3600)); 
```

Vous pouvez également obtenir la date et l'heure en utilisant un fuseau horaire UTC en utilisant la méthode "now_utc()" au lieu de "Local::now()".

## Voir aussi

- Documentation de la bibliothèque Chrono: https://docs.rs/chrono
- Article sur la manipulation des dates et heures en Rust: https://stevedonovan.github.io/rust-gentle-intro/8-dates-times.html