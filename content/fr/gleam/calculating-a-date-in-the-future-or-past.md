---
title:                "Calculer une date dans le futur ou le passé"
html_title:           "Gleam: Calculer une date dans le futur ou le passé"
simple_title:         "Calculer une date dans le futur ou le passé"
programming_language: "Gleam"
category:             "Gleam"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fr/gleam/calculating-a-date-in-the-future-or-past.md"
---

{{< edit_this_page >}}

## Qu'est-ce et Pourquoi ?
Calculer une date passée ou future, c'est manipuler les dates en les avançant ou en les rembobinant dans le temps. C'est souvent nécessaire pour programmer des rappels, des notifications ou des évènements planifiés pour des applications.

## Comment faire :
Voici rapidement comment vous pouvez calculer la date future avec Gleam :

```gleam
import gleam/calendar.{add_days, Date}
import gleam/time.{current_date, from_ok}

fn future_date(days: Int) {
  let today = from_ok(current_date())
  add_days(today, days)
}
```

Pour obtenir la date trois jours à partir d'aujourd'hui :
```gleam
future_date(3)
```
Sortie : `Ok(#Date(year: 2022, month: 8, day: 10))`

## Exploration Approfondie :
Historiquement, calculer la date future a été un casse-tête à cause des années bissextiles, des fuseaux horaires et d'autres nuances. Avec Gleam, le calcul est simplifié en utilisant le module `calendar`.

Les alternatives à Gleam pour cette tâche pourraient être des langages tels que Python ou Java, qui ont des bibliothèques de date et de temps robustes.

En ce qui concerne les détails d'implémentation, cette action est effectuée en ajoutant simplement le nombre spécifié de jours à la date actuelle. Cela repose sur la capacité de Gleam à gérer correctement le roulement des mois et des années.

## Voir Aussi :
Pour plus d'informations, consultez les liens suivants :

- Documentation officielle de Gleam : https://gleam.run/book/tour/dates-and-time.html
- GitHub de Gleam pour des exemples supplémentaires : https://github.com/gleam-lang/gleam
- Article de blog approfondi sur les dates et heures en Gleam : https://gleam.run/news/dates-and-times-in-gleam/