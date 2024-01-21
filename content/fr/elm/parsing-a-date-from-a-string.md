---
title:                "Analyse d'une date à partir d'une chaîne de caractères"
date:                  2024-01-20T15:35:47.032444-07:00
html_title:           "Arduino: Analyse d'une date à partir d'une chaîne de caractères"
simple_title:         "Analyse d'une date à partir d'une chaîne de caractères"
programming_language: "Elm"
category:             "Elm"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fr/elm/parsing-a-date-from-a-string.md"
---

{{< edit_this_page >}}

## What & Why?
Le parsing de dates, c'est convertir une date en texte vers un format utilisable par le programme. Les développeurs en ont besoin pour gérer des dates dans les applications, comme les réservations ou les historiques.

## How to:
Elm utilise le package `elm/time` pour gérer les dates. Voici un exemple de conversion d'une string en date :

```Elm
import Time
import Time.Posix exposing (Posix)
import Date exposing (Date)

stringToDate : String -> Result String Posix
stringToDate dateStr =
    case Date.fromIsoString8601 dateStr of
        Ok date ->
            Ok (Date.posix date)

        Err error ->
            Err "Format de date invalide"

-- Usage
result : Result String Posix
result =
    stringToDate "2023-04-10"
```

Si vous lancez ce code avec "2023-04-10", `result` sera un `Ok <posix value>` qui correspond à la date convertie. Si le format est incorrect, vous obtiendrez `Err "Format de date invalide"`.

## Deep Dive
Historiquement, Elm a toujours cherché à simplifier la gestion des temps et des dates via des abstractions haut niveau. Comparé à JavaScript où `Date.parse()` existe, Elm nécessite d'installer des packages pour cette fonctionnalité. Cela garantit que le parsing des dates est traité explicitement par le développeur. En dehors du package `elm/time`, on peut utiliser `justinmimbs/date` pour des fonctionnalités de parsing plus avancées. Dans l'implémentation, Elm oblige au traitement des erreurs, conduisant à un code plus sûr.

## See Also
- Package [`justinmimbs/date`](https://package.elm-lang.org/packages/justinmimbs/date/latest/)
- Elm Guide sur les [Time Zones](https://guide.elm-lang.org/effects/time.html)