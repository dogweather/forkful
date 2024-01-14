---
title:                "Elm: Transformation d'une date en chaîne de caractères"
programming_language: "Elm"
category:             "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fr/elm/converting-a-date-into-a-string.md"
---

{{< edit_this_page >}}

## Pourquoi

Si vous êtes nouveau dans le monde de la programmation Elm, vous vous demandez peut-être pourquoi il serait important de convertir une date en une chaîne de caractères. Eh bien, cela peut sembler être une tâche simple et banale, mais cela peut être très utile lorsque vous travaillez avec des données temporelles dans votre application. La conversion d'une date en une chaîne de caractères peut vous permettre de l'afficher de manière conviviale pour l'utilisateur ou d'effectuer des opérations de recherche et de tri sur des dates.

## Comment faire

Pour convertir une date en une chaîne de caractères en Elm, vous pouvez utiliser la fonction `toString` du module `Time`. Elle prend en paramètre une date et un format de chaîne de caractères et renvoie la date au format spécifié.

```
-- Import du module Time
import Time exposing (Date, Day(..), Apr, Year, toString)

-- Définition d'une date
date = Date 2021 4 1

-- Convertir en chaîne de caractères au format "mm/dd/yyyy"
toString "mm/dd/yyyy" date -- Output : "04/01/2021"
```

Vous pouvez également utiliser le module `Time.Format` pour plus de flexibilité dans la mise en forme de votre date en chaîne de caractères. Ce module offre également plus d'options pour les mois et jours abrégés, ainsi que pour les fuseaux horaires.

```
-- Import du module Time et Time.Format
import Time exposing (Date, Day(..), Apr, Year)
import Time.Format exposing (..)

-- Définition d'une date
date = Date 2021 4 1

-- Convertir en chaîne de caractères au format "JJJ d, AAAA"
format format {year = Full, month = ThreeLetter, day = Digit, hour = Numeric 12, minute = TwoDigit, second = None, amPm = Capitalized, dayName = Abbreviated, zone = TimezoneName} date -- Output : "Apr 1, 2021"
```

## Plongée en profondeur

Il est important de noter que lors de la conversion d'une date en chaîne de caractères, il est crucial d'utiliser le bon format pour éviter toute confusion ou tout dysfonctionnement de votre application. Par exemple, si vous utilisez le format "mm/dd/yyyy" et que vous utilisez une date avec le mois en lettres, cela peut entraîner une erreur lors de l'affichage de la date. Il est donc important de comprendre les différents formats disponibles et de choisir celui qui convient le mieux à votre projet.

## Voir aussi

- [Documentation sur le module Time](https://package.elm-lang.org/packages/elm/time/latest/Time)
- [Documentation sur le module Time.Format](https://package.elm-lang.org/packages/elm-lang/core/latest/Time-Format)