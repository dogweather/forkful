---
title:                "Elm: Comparer deux dates"
simple_title:         "Comparer deux dates"
programming_language: "Elm"
category:             "Elm"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fr/elm/comparing-two-dates.md"
---

{{< edit_this_page >}}

## Pourquoi

Comparer deux dates est une pratique essentielle lors de la programmation en Elm. Cela peut être utile pour trier des données, créer des rappels ou vérifier l'ancienneté d'une information. Dans cet article, nous allons voir comment comparer facilement et efficacement deux dates en utilisant Elm.

## Comment faire

Pour comparer deux dates en Elm, nous pouvons utiliser la fonction `compare` du module `Date`. Cette fonction prend deux arguments de type `Date` et renvoie un `Order` qui peut être `LT` (inférieur), `EQ` (égal) ou `GT` (supérieur). Voici un exemple de code pour comparer deux dates :

```Elm
firstDate = Date.fromYearMonthDay 2020 05 15
secondDate = Date.fromYearMonthDay 2021 05 20

comparison = Date.compare firstDate secondDate

case comparison of 
    LT ->
        "La première date est plus ancienne que la deuxième."
    
    EQ ->
        "Les deux dates sont identiques."
    
    GT ->
        "La première date est plus récente que la deuxième."
```

Dans cet exemple, nous créons deux dates différentes et nous utilisons la fonction `compare` pour obtenir le résultat de la comparaison. Ensuite, nous utilisons un `case` pour traiter les différents cas possibles et afficher un message correspondant.

## Plongeons plus profondément

Lorsque nous utilisons `compare`, le résultat est basé sur la comparaison des timestamps des deux dates. Un timestamp représente le nombre de millisecondes écoulées depuis le 1er janvier 1970 à minuit en temps universel (UTC). Cela signifie que si deux dates sont exactement identiques jusqu'au millisecondes, elles seront considérées comme égales.

De plus, si l'une des dates est vide, la fonction renverra automatiquement `EQ`. Enfin, il est important de noter que la fonction `compare` ne prend pas en compte le fuseau horaire. Si les dates ont des fuseaux horaires différents, il est préférable de les convertir en UTC avant de les comparer.

## Voir aussi

- [Documentation officielle d'Elm sur le module Date](https://package.elm-lang.org/packages/elm/time/latest/Date)
- [Différentes façons de comparer des dates en JavaScript](https://www.sitepoint.com/comparing-datestimes-using-javascript/)
- [Article sur la gestion des dates en français avec Elm](https://www.theodo.fr/blog/2021/02/gestion-dates-elm/)