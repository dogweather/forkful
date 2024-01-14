---
title:                "Gleam: Obtenir la date actuelle"
simple_title:         "Obtenir la date actuelle"
programming_language: "Gleam"
category:             "Gleam"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fr/gleam/getting-the-current-date.md"
---

{{< edit_this_page >}}

## Pourquoi

Il est important de pouvoir accéder à la date actuelle lors du développement d'un programme en Gleam. Cela peut être utile pour afficher la date dans une interface utilisateur ou pour effectuer des calculs basés sur la date.

## Comment faire

Pour obtenir la date actuelle en Gleam, il suffit d'utiliser la fonction `Date.now()` avec la bibliothèque standard `gleam/time`. Voici un exemple :

```Gleam
import gleam/time

let now = Date.now()
```

Vous pouvez également préciser un fuseau horaire spécifique en utilisant la fonction `Date.now_with_timezone()` et en passant en paramètre le fuseau horaire souhaité. Par exemple :

```Gleam
import gleam/time

let now = Date.now_with_timezone("Europe/Paris")
```

Lorsque vous exécutez ces codes, vous obtiendrez un résultat similaire à `2021-10-10T19:30:00Z`. Il est important de noter que la valeur renvoyée par la fonction `Date.now()` est un enregistrement avec des champs tels que `year`, `month`, `day`, `hour`, `minute` et `second`. Ces champs peuvent être utilisés pour afficher la date de manière plus spécifique dans votre programme.

## Plongée en profondeur

Le module `gleam/time` fournit diverses autres fonctions utiles pour travailler avec les dates telles que `Date.from_utc_nanos()` pour convertir une heure d'un fuseau horaire spécifique en un enregistrement de date, et `Date.from_date()` pour construire un enregistrement de date à partir de valeurs spécifiques pour chaque champ (année, mois, jour, heure, etc.).

Il est également possible de formater l'affichage de la date en utilisant la fonction `format()` avec des options telles que `year`, `month`, `day`, `hour`, `minute` et `second` pour personnaliser l'affichage de la date selon vos besoins.

## Voir aussi

- Documentation officielle de Gleam sur la gestion des dates : [https://gleam.run/libraries/time](https://gleam.run/libraries/time)
- Tutoriel sur la manipulation des dates en Gleam : [https://gleam.run/news/dates-in-gleam](https://gleam.run/news/dates-in-gleam)
- Bibliothèque de fuseaux horaires prise en charge par Gleam : [https://github.com/laukvik/gleam-timezone](https://github.com/laukvik/gleam-timezone)