---
title:                "Obtenir la date actuelle"
date:                  2024-01-20T15:16:28.153378-07:00
html_title:           "C: Obtenir la date actuelle"
simple_title:         "Obtenir la date actuelle"

tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fr/rust/getting-the-current-date.md"
---

{{< edit_this_page >}}

## What & Why?
Qu'est-ce que c'est ? Obtenir la date actuelle, c'est récupérer la date du jour selon notre calendrier. Pourquoi ? Les dévs le font pour des logs, des timestamps, des features dépendantes du temps, etc.

## How to:
Pour avoir la date du jour en Rust, on utilise souvent `chrono` - une crate qui gère le temps. Voilà comment :

```Rust
extern crate chrono;
use chrono::prelude::*;

fn main() {
    let now = Local::now();
    println!("{}", now.format("%d/%m/%Y").to_string()); // Format français: jour/mois/année
}
```

Tu auras une sortie genre :

```
04/04/2023
```

Simple, non ?

## Deep Dive
Avant `chrono`, Rust avait des fonctions de temps assez basiques. `chrono` est devenu le choix par défaut, car il est riche et fiable. Alternativement, dans la bibliothèque standard, `std::time` offre quelques outils, mais moins spécifiques pour les dates.

`chrono` gère les timezones, une vraie galère sans elle. En interne, ça calcule des timestamps Unix et fait des conversions entre différents formats. Pratique !

Si tu veux éviter une dépendance externe, Rust 1.47+ a une lib standard (`std::time`) qui se bonifie avec le temps. Mais sérieusement, `chrono` te sauvera plein de temps.

## See Also
- [La documentation `chrono`](https://docs.rs/chrono/*): Tout ce que tu dois savoir.
- [La librairie de temps standard de Rust (`std::time`)](https://doc.rust-lang.org/std/time/): Si tu décides de l'utiliser.
- [Le livre Rust](https://doc.rust-lang.org/book/): Pour les bases et plus.
