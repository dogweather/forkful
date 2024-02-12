---
title:                "Conversion d'une date en chaîne de caractères"
date:                  2024-01-20T17:37:38.694089-07:00
model:                 gpt-4-1106-preview
simple_title:         "Conversion d'une date en chaîne de caractères"

tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fr/rust/converting-a-date-into-a-string.md"
---

{{< edit_this_page >}}

## Quoi & Pourquoi ?
Convertir une date en chaîne de caractères, c'est transformer une représentation de date en texte lisible. Les programmeurs le font généralement pour afficher des dates aux utilisateurs ou pour les enregistrer dans un format standardisé.

## Comment faire :
```Rust
use chrono::{DateTime, Utc, Local, NaiveDateTime};

fn main() {
    // Date/heure actuelle en UTC
    let now_utc: DateTime<Utc> = Utc::now();
    println!("Date/heure UTC: {}", now_utc.to_rfc3339());

    // Date/heure actuelle locale
    let now_local: DateTime<Local> = Local::now();
    println!("Date/heure locale: {}", now_local.format("%d-%m-%Y %H:%M:%S").to_string());

    // Convertir une NaiveDateTime sans fuseau horaire en String
    let naive_datetime = NaiveDateTime::from_timestamp(1041379200, 0);
    println!("Date/heure naive: {}", naive_datetime.format("%Y-%m-%d %H:%M:%S").to_string());
}
```
Sortie :
```
Date/heure UTC: 2023-04-01T12:34:56+00:00
Date/heure locale: 01-04-2023 14:34:56
Date/heure naive: 2003-01-01 00:00:00
```

## Exploration
Historiquement, Rust utilise la crate `chrono` pour la gestion des dates, bien que la bibliothèque standard propose des modules pour le temps. `chrono` offre plus de fonctionnalités pour le formatage et l'analyse de dates qu'une simple conversion en chaînes de caractères. Alternativement, on peut utiliser la bibliothèque `time`, mais `chrono` reste la référence de la communauté pour une manipulation riche et précise des dates.

La manipulation des dates implique souvent la gestion des fuseaux horaires avec `DateTime`, ou le formatage des dates et heures sans time zone avec `NaiveDateTime`. Il est crucial de choisir le bon type pour représenter correctement la date en fonction de son utilisation.

Quand on convertit une date en chaîne de caractères, on peut utiliser des formats prédéfinis comme `RFC3339` ou personnaliser le format en spécifiant un modèle, comme `"%d-%m-%Y %H:%M:%S"`. Les formats personnalisés sont définis suivant la syntaxe de la libc’s `strftime`.

## Voir aussi
- Documentation de `chrono`: https://docs.rs/chrono/latest/chrono/
- Documentation de `time`: https://docs.rs/time/latest/time/
- Syntaxe de formatage de `strftime`: http://man7.org/linux/man-pages/man3/strftime.3.html
