---
title:                "Calculer une date dans le futur ou le passé"
html_title:           "Rust: Calculer une date dans le futur ou le passé"
simple_title:         "Calculer une date dans le futur ou le passé"
programming_language: "Rust"
category:             "Rust"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fr/rust/calculating-a-date-in-the-future-or-past.md"
---

{{< edit_this_page >}}

## Quoi & Pourquoi?
Calculer une date dans le futur ou le passé, c'est simplement déterminer une date qui est soit avant soit après une date donnée. Les programmeurs le font souvent pour des besoins de suivi de tâches, de planification et d’analyse historique.

## Comment faire :
Voici un exemple basique de calcul d'une date dans le futur dans Rust:

```Rust
extern crate chrono;
use chrono::{DateTime, Duration, Utc};

fn main() {
  let now: DateTime<Utc> = Utc::now();
  let future_date = now.checked_add_signed(Duration::days(5)).unwrap();
  println!("Dans 5 jours, la date sera : {}", future_date);
}
```
Exécution de ce code donne un résultat qui ressemble à:

```
Dans 5 jours la date sera: 2025-09-18T09:28:00.434187Z
```

## Plongée en profondeur
Historiquement, le calcul de dates a longtemps été un défi dans le domaine de l'informatique. De nombreux bugs et problèmes ont été rencontrés dans le passé. Avez-vous entendu parler du bug de l'an 2000 ?

Il existe plusieurs alternatives pour calculer une date dans le futur ou le passé en Rust. Vous pourriez utiliser la bibliothèque `time` qui propose des méthodes similaires à `chrono`.

Les détails de l'implémentation liés au calcul des dates dans `chrono` sont complexes. `chrono` gère les durées, les fuseaux horaires, les périodes ainsi que les formats d'affichage et de parsing de dates.

## Voir aussi
Pour plus d'informations, consultez les docs et exemples de la librairie `chrono` ici: [chrono - Rust](https://docs.rs/chrono/0.4.19/chrono/) et pour la librairie `time` ici: [time - Rust](https://docs.rs/time/0.1.42/time/).