---
title:                "Obtenir la date actuelle"
html_title:           "Elm: Obtenir la date actuelle"
simple_title:         "Obtenir la date actuelle"
programming_language: "Elm"
category:             "Elm"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fr/elm/getting-the-current-date.md"
---

{{< edit_this_page >}}

## Qu'est-ce que c'est et pourquoi le faire?

Obtenir la date actuelle est une fonctionnalité utile pour de nombreux programmes. Cela permet aux programmeurs de travailler avec des informations basées sur le temps, telles que des rappels, des délais ou des enregistrements de données. Cela peut également aider à garder une piste de l'historique des activités d'une application.

## Comment faire:

Pour obtenir la date actuelle en Elm, nous pouvons utiliser la fonction `Date.now` de la bibliothèque `Time`, qui renvoie un `Time.Posix` représentant l'instant présent en tant que nombre de millisecondes depuis le 1er janvier 1970 00:00:00 GMT.

```Elm
import Time exposing (..)

currentDate : Time.Posix
currentDate = Date.now
```

Le résultat sera un nombre tel que `1517987890000`, qui peut être converti en une forme plus lisible avec d'autres fonctions de la bibliothèque `Time` si nécessaire.

## Plongée plus profonde:

La prise en charge du temps et de la date dans les langages de programmation est apparue pour la première fois avec FORTRAN IV en 1962, avec la fonction `DATE` qui renvoie la date actuelle dans un format spécifié. Depuis lors, de nombreux langages ont implémenté leurs propres fonctions pour gérer la date et l'heure.

Dans Elm, outre la fonction `Date.now`, vous pouvez également utiliser la fonction `Date.fromTime` pour convertir un `Time.Posix` en une représentation de date lisible. Vous pouvez également utiliser la bibliothèque [chrono](https://package.elm-lang.org/packages/elm/time/latest/Time-Chrono) pour des manipulations de date plus avancées.

## Voir aussi:

- [Documentation Elm - Date.now](https://package.elm-lang.org/packages/elm/time/latest/Time#now)
- [Documentation Elm - Date.fromTime](https://package.elm-lang.org/packages/elm/time/latest/Time#fromTime)
- [Documentation Elm - Chrono](https://package.elm-lang.org/packages/elm/time/latest/Time-Chrono#Date)