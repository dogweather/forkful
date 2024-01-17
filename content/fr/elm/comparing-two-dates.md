---
title:                "Comparaison de deux dates"
html_title:           "Elm: Comparaison de deux dates"
simple_title:         "Comparaison de deux dates"
programming_language: "Elm"
category:             "Elm"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fr/elm/comparing-two-dates.md"
---

{{< edit_this_page >}}

## Qu'est-ce que la comparaison de deux dates et pourquoi les programmeurs le font-ils ?

La comparaison de deux dates est simplement le fait de comparer les valeurs de deux dates pour déterminer leur relation chronologique. Par exemple, on peut comparer deux dates pour savoir laquelle est plus récente ou si elles sont équivalentes. Les programmeurs font cela pour traiter de manière appropriée les données temporelles dans leurs programmes et s'assurer que les actions sont effectuées dans l'ordre correct.

## Comment faire :

```
Elm.Date.compare date1 date2
```

La fonction `compare` de la bibliothèque Elm Date compare les valeurs de deux dates et renvoie un `Order` indiquant leur relation. Voici quelques exemples de sortie possibles :

- `LT` : si la `date1` est avant `date2`
- `GT` : si la `date1` est après `date2`
- `EQ` : si la `date1` est égale à `date2`

Vous pouvez également utiliser cette fonction pour trier une liste de dates en utilisant la fonction `List.sortBy` avec `compare` comme argument.

## Plongée en profondeur :

La comparaison de deux dates est souvent utilisée dans les programmes de planification et de suivi du temps, ainsi que dans les applications liées aux événements et aux tâches.

Au lieu d'utiliser `Date.compare`, certains programmeurs peuvent utiliser la fonction `Date.isBefore` ou `Date.isAfter` pour une comparaison spécifique. Il est également important de noter que la bibliothèque Elm Date utilise le nombre de millisecondes écoulées depuis le 1er janvier 1970 pour représenter une date, ce qui peut différer des autres langages de programmation.

## Voir aussi :

- Documentation officielle de la bibliothèque Elm Date : https://package.elm-lang.org/packages/elm/core/latest/Date
- Article sur la représentation des dates en informatique : https://fr.wikipedia.org/wiki/Date_%28informatique%29