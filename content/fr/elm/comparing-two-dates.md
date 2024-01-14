---
title:                "Elm: Comparer deux dates"
programming_language: "Elm"
category:             "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fr/elm/comparing-two-dates.md"
---

{{< edit_this_page >}}

## Pourquoi

La comparaison de deux dates est une tâche courante en programmation. Cela peut être utile pour trier des données chronologiquement, vérifier si une date se situe avant ou après une autre, ou pour calculer la différence entre deux dates. Dans cet article, nous allons voir comment réaliser cette opération en utilisant le langage Elm.

## Comment faire

Nous allons commencer par définir deux dates sous forme de variables. Dans cet exemple, nous allons utiliser le package `Date` de la bibliothèque standard d'Elm.

```elm
import Date exposing (Date)

-- Définir la première date
date1 : Date
date1 = Date.fromIsoString "2020-01-01"

-- Définir la deuxième date
date2 : Date
date2 = Date.fromIsoString "2021-01-01"
```

Ensuite, nous pouvons utiliser les fonctions fournies par le package `Date` pour comparer ces deux dates. Par exemple, pour vérifier si `date1` se situe avant `date2`, nous pouvons utiliser la fonction `Date.before` qui renvoie une valeur booléenne.

```elm
Date.before date1 date2     -- renverra True
```

Pour calculer la différence entre ces deux dates en termes de jours, nous pouvons utiliser la fonction `Date.diffInDays`.

```elm
Date.diffInDays date1 date2  -- renverra 366 (une année bissextile)
```

Il existe également d'autres fonctions comme `Date.after` pour vérifier si une date se situe après une autre, `Date.isLeapYear` pour vérifier si une année est bissextile, etc. N'hésitez pas à explorer la documentation du package `Date` pour en savoir plus.

## Approfondissement

Lorsque vous comparez deux dates, il est important de prendre en compte les différences de fuseau horaire et les années bissextiles. En utilisant le package `Date`, vous n'aurez pas à vous soucier de ces problèmes, car il les gère automatiquement pour vous.

De plus, vous pouvez également utiliser la fonction `Date.fromString` pour définir une date à partir d'une chaîne de caractères au format souhaité, ce qui peut être utile si vous obtenez les dates à partir d'une source externe.

## Voir aussi

- Documentation du package `Date` : https://package.elm-lang.org/packages/elm/time/latest/Date
- Démo en ligne : https://ellie-app.com/3hmpQjt7WHqa1