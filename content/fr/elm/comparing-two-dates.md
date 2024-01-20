---
title:                "Comparer deux dates"
html_title:           "Clojure: Comparer deux dates"
simple_title:         "Comparer deux dates"
programming_language: "Elm"
category:             "Elm"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fr/elm/comparing-two-dates.md"
---

{{< edit_this_page >}}

## Quoi et Pourquoi?

Comparer deux dates signifie vérifier quelle date est antérieure, postérieure ou si les deux sont identiques. Les programmeurs l'utilisent fréquemment pour des tâches telles que le tri d'événements, le calcul de l'âge ou la détermination de la durée.

## Comment faire:

En Elm, nous pouvons utiliser les fonctions intégrées `Date.before`, `Date.after` et `Date.equals` pour comparer deux dates. 

```Elm
import Date exposing (..)

date1 = Date.fromIsoString "2020-01-01T00:00:00Z"
date2 = Date.fromIsoString "2020-12-01T00:00:00Z"

isBefore = Date.before date1 date2
isAfter = Date.after date1 date2
isEqual = Date.equals date1 date2 
```

Cela fournira respectivement `True`, `False` et `False` puisque la date1 se trouve avant la date2.

## Plongée Profonde:

Historiquement, la comparaison de dates est un problème ancien en informatique. Des solutions variées et parfois complexes ont été utilisées. Dans Elm, il suffit d'utiliser le module `Date` pour une comparaison plus simple et plus précise.

Cependant, d'autres alternatives existent. On pourrait, par exemple, convertir les deux dates en millisecondes depuis l'époque Unix et les comparer. Cette approche plus bas niveau a l'avantage d’être indépendante de la librairie.

Au niveau de l’implémentation, `Date.before`, `Date.after` et `Date.equals` utilisent l'opérateur de comparaison (`<`, `>`, `==`) sur la valeur en millisecondes des dates.

## Voir aussi:

Pour en savoir plus sur le module Date, consultez:

- [Documentation Elm pour le module Date](http://package.elm-lang.org/packages/elm/time/latest/Date)

Pour une exploration plus étendue de la comparaison de temps, vous pouvez consulter:
