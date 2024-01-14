---
title:    "Elm: Obtenir la date actuelle"
keywords: ["Elm"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/fr/elm/getting-the-current-date.md"
---

{{< edit_this_page >}}

## Pourquoi

Dans la programmation, il est souvent nécessaire de connaître la date actuelle. Cela peut servir à différents usages tels que l'affichage sur une interface utilisateur ou à des fins de traitement de données. Dans cet article, nous allons voir comment obtenir facilement la date actuelle en utilisant le langage de programmation Elm.

## Comment Faire

Pour obtenir la date actuelle en Elm, nous allons utiliser la fonction `Time.now`. Cette fonction retourne la date et l'heure actuelles sous forme de type `Posix` qui représente le temps en millisecondes depuis le 1er janvier 1970.

```
Elm
import Time exposing (now)

currentTime : Task x Time.Posix
currentTime =
  now
```

Pour afficher la date dans un format plus lisible pour l'utilisateur, nous pouvons utiliser la fonction `Time.Date`, qui prend en paramètre un `Posix` et retourne une structure de données contenant les informations de date.

```
Elm
import Time exposing (..)

date : Date
date =
  Time.millisToPosix 1568838140000
  |> Time.millisToDate
```

Avec cette structure de données, nous pouvons accéder facilement aux différentes informations de date telles que le jour, le mois et l'année.

```
Elm
date.month -- 9
date.day -- 18
date.year -- 2019
```

## En Profondeur

Sous le capot, la fonction `Time.now` utilise la fonction `JS.Date.now` du langage JavaScript pour obtenir la date actuelle. Cela signifie que la date renvoyée peut varier légèrement en fonction de l'emplacement géographique de l'utilisateur.

Il est également important de noter que la fonction `Time.now` retourne une action de tâche `Task` plutôt qu'une valeur réelle de date. Cela signifie que nous devons utiliser `Task.perform` ou un autre opérateur de `Task` pour récupérer la valeur de date réelle.

## Voir Aussi

- Documentation officielle Elm sur Time: https://package.elm-lang.org/packages/elm/time/latest/
- Tutoriel sur la gestion des dates en Elm: https://www.programming-motherfucker.com/blog/86/the-ultimate-guide-to-date-and-time-manipulation-in-elm