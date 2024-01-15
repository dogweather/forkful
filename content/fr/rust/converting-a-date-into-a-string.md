---
title:                "Convertir une date en chaîne de caractères"
html_title:           "Rust: Convertir une date en chaîne de caractères"
simple_title:         "Convertir une date en chaîne de caractères"
programming_language: "Rust"
category:             "Rust"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fr/rust/converting-a-date-into-a-string.md"
---

{{< edit_this_page >}}

## Pourquoi

Si vous travaillez avec des données de dates dans votre code Rust, il se peut que vous ayez besoin de convertir ces données en une chaîne de caractères pour l'afficher ou pour les transmettre à un autre système. Heureusement, Rust offre des outils simples et efficaces pour effectuer cette conversion.

## Comment faire

Voici un exemple de code qui convertit une date en chaîne de caractères en utilisant la bibliothèque standard de Rust :

```Rust
use std::time::Duration;
use chrono::{Utc, Timelike};

let date = Utc::now();
let string_date = date.to_rfc3339();

println!("La date actuelle est : {}", string_date);
```

L'exemple ci-dessus utilise la bibliothèque "chrono" pour créer une variable contenant la date et l'heure actuelles en utilisant le fuseau horaire UTC. Ensuite, la fonction to_rfc3339() convertit cette date en une chaîne de caractères au format RFC 3339, qui est une norme couramment utilisée pour représenter les dates et heures en texte.

Lorsque la chaîne de caractères est imprimée à l'écran, vous devriez avoir un résultat similaire à ceci :

```Rust
La date actuelle est : 2021-09-19T20:20:21Z
```

## Plongée profonde

Si vous souhaitez personnaliser le format de la chaîne de caractères contenant la date, vous pouvez utiliser la fonction format() de la bibliothèque "chrono". Cette fonction vous permet de spécifier un modèle pour le formatage de la date, par exemple :

```Rust
use std::time::Duration;
use chrono::{Utc, Timelike};

let date = Utc::now();
let string_date = date.format("%d/%m/%Y %H:%M:%S").to_string();

println!("La date actuelle est : {}", string_date);
```

Dans cet exemple, la date sera formatée dans le format "jour/mois/année heure:minute:seconde". La fonction to_string() est utilisée pour convertir la date formatée en chaîne de caractères.

Vous pouvez également modifier le fuseau horaire de la date avant de la convertir en chaîne de caractères, en utilisant la fonction with_timezone() de la bibliothèque "chrono". Cela peut être utile si vous travaillez avec des dates et heures dans différents fuseaux horaires.

## Voir aussi

- Documentation de la bibliothèque "chrono" : https://docs.rs/chrono/latest/chrono/index.html
- Tutoriel sur la manipulation des dates et heures en Rust : https://dev.to/thebrandroid/rust-for-web-developers-working-with-date-time-243k