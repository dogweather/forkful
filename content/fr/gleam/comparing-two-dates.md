---
title:                "Comparaison de deux dates"
html_title:           "Gleam: Comparaison de deux dates"
simple_title:         "Comparaison de deux dates"
programming_language: "Gleam"
category:             "Gleam"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fr/gleam/comparing-two-dates.md"
---

{{< edit_this_page >}}

## Qu'est-ce que c'est & Pourquoi?

Comparing two dates, or "dater" en français, is a common task for programmers when working with time and dates. It involves comparing two dates to determine their relative order, whether one date is before, after, or the same as the other. This is important for tasks such as sorting events in a calendar or determining the age of a user.

## Comment faire:

Pour comparer deux dates en Gleam, nous pouvons utiliser la fonction `Date.compare`, qui prend deux paramètres de type `Date` et renvoie un nombre entier. Ce nombre sera négatif si la première date est avant la deuxième, positif si elle est après, et zéro si les deux dates sont identiques.

```
Gleam.dispatch(
  case Date.compare(
    Date.from_gregorian(2021, 10, 15),
    Date.from_gregorian(2021, 10, 13)
  ) {
    -1 => "La première date est avant la deuxième"
    1 => "La première date est après la deuxième"
    _ => "Les deux dates sont identiques"
  }
)
```

## Plongée en profondeur:

Bien que la comparaison de dates puisse sembler simple, il y a des nuances à prendre en compte, en particulier lors de l'utilisation de différents calendriers et fuseaux horaires. Il existe également d'autres approches pour comparer les dates, telles que l'utilisation de timestamps ou de bibliothèques externes spécialisées. En interne, Gleam utilise le calendrier gregorien pour la gestion des dates.

## Voir aussi:

Pour en savoir plus sur les dates en Gleam, consultez la documentation officielle sur le type `Date` et ses méthodes. Vous pouvez également explorer d'autres concepts connexes tels que les durations (durées) et les time zones (fuseaux horaires). Bon codage !