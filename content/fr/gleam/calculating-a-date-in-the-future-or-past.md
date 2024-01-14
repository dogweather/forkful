---
title:                "Gleam: Calculer une date dans le futur ou le passé"
simple_title:         "Calculer une date dans le futur ou le passé"
programming_language: "Gleam"
category:             "Gleam"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fr/gleam/calculating-a-date-in-the-future-or-past.md"
---

{{< edit_this_page >}}

# Pourquoi

Vous êtes-vous déjà demandé comment programmer une date dans le futur ou dans le passé ? Peut-être que vous devez automatiser des tâches ou que vous cherchez à anticiper des événements à venir. Quelle que soit la raison, dans cet article, nous allons plonger dans la programmation de dates en utilisant le langage de programmation fonctionnelle Gleam.

# Comment Faire

Pour calculer une date dans le futur ou dans le passé en utilisant Gleam, nous utiliserons la fonction `Date.add/2` de la bibliothèque standard. Cette fonction prend deux arguments : une date de départ et une durée représentée en nombre de millisecondes. Voyons un exemple concret :

```Gleam
import gleam/time.{ Date }

let start = Date.new(
  year: 2021,
  month: 8,
  day: 10
) in
let days = 7 * 24 * 60 * 60 * 1000 in
let end = Date.add(start, days)

// Output: Date.new(year: 2021, month: 8, day: 17)
```

Nous avons d'abord créé une date de départ en utilisant la fonction `Date.new/3` en spécifiant l'année, le mois et le jour. Ensuite, nous avons calculé la durée en millisecondes en multipliant le nombre de jours par 24 heures, 60 minutes, 60 secondes et 1000 millisecondes. Enfin, nous avons utilisé la fonction `Date.add/2` pour ajouter la durée à la date de départ et obtenir la date souhaitée dans le futur.

Si nous voulons calculer une date dans le passé, il suffit de donner une durée négative à la fonction `Date.add/2`.

# Plongée en Profondeur

Maintenant que nous avons vu comment utiliser la fonction `Date.add/2`, il est important de comprendre comment les dates sont représentées en Gleam. Les dates sont représentées sous forme de tuples contenant les informations suivantes : année, mois, jour, heure, minute, seconde, milliseconde. Par exemple, la date 10 août 2021 est représentée comme suit :

`{2021, 8, 10, 0, 0, 0, 0}`

De plus, la bibliothèque standard de Gleam fournit également d'autres fonctions pour manipuler et formater les dates en utilisant le type `Date`.

# Voir Aussi

- [Documentation sur la bibliothèque standard de Gleam](https://gleam.run/documentation/standard-libraries/#date)
- [Guide officiel de programmation en Gleam](https://gleam.run/book/tour.html#dates)
- [Exemples de manipulation de dates en Gleam](https://github.com/gleam-lang/gleam/blob/master/examples/time/main.gleam)