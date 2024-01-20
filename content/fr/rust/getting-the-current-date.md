---
title:                "Obtenir la date actuelle"
html_title:           "Bash: Obtenir la date actuelle"
simple_title:         "Obtenir la date actuelle"
programming_language: "Rust"
category:             "Rust"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fr/rust/getting-the-current-date.md"
---

{{< edit_this_page >}}

## Quoi & Pourquoi?
Prendre la date actuelle signifie obtenir l'information sur la date et l'heure à ce moment précis. Les programmeurs font cela pour suivre des événements, journaliser les informations, ou ajouter des caractéristiques temporelles à leurs applications.

## Comment faire:
Rust nous permet d'obtenir la date actuelle à travers le paquet Chrono. Voici un petit bout de code pour vous montrer comment:
```Rust
use chrono::{DateTime, Local};

fn main() {
    let maintenant: DateTime<Local> = Local::now();
    println!("{}", maintenant);
}
```
Lorsque vous exécutez ce code, vous obtiendrez quelque chose comme ceci:
```Rust
2022-01-07 22:34:26.829074 +01:00
```

## Approfondissement: 

Récupérer la date actuelle est une fonctionnalité ancienne et indispensable dans la programmation. Dans le passé, les programmeurs exploitaient les APIs système pour cela mais Rust, dans sa quête pour la sécurité, fournir un paquet riche en fonctionnalités pour gérer tout ce qui est lié au temps.

Il y a bien sûr des alternatives à Chrono, comme le paquet time, ou même des manipulations de bas niveau. Cependant, Chrono reste la solution la plus complète et la plus facile à utiliser.

L'implémentation du paquet Chrono fait abstractions des détails de bas niveau, et nous donne une API facile à utiliser. Il gère aussi pour nous des éléments complexes comme les fuseaux horaires.

## A Voir Aussi:

Pour plus d'informations, vous pouvez visiter:

- Documentation du Chrono: https://docs.rs/chrono/0.4.19/chrono/
- Rust by Example: https://doc.rust-lang.org/stable/rust-by-example/std_misc/chrono.html
- Le livre Rust (FR): https://doc.rust-lang.org/book/fr-FR/title-page.html