---
title:    "Elm: Calculer une date dans le futur ou le passé"
keywords: ["Elm"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/fr/elm/calculating-a-date-in-the-future-or-past.md"
---

{{< edit_this_page >}}

## Pourquoi

Dans la programmation, il est souvent nécessaire de calculer des dates dans le futur ou dans le passé. Par exemple, pour afficher la date de naissance d'un utilisateur dans un format plus lisible ou pour planifier des tâches à une date précise, il est important de comprendre comment calculer des dates. Dans cet article, nous allons explorer comment utiliser Elm pour calculer une date dans le futur ou dans le passé.

## Comment faire

Pour commencer, nous allons décomposer le processus en plusieurs étapes simples. Tout d'abord, nous devons créer une fonction qui prendra en compte la date actuelle et le nombre de jours à ajouter ou à soustraire. Nous pouvons utiliser la fonction `add` ou `sub` de la bibliothèque `Time` d'Elm pour cela. Ensuite, nous devons formater la date pour qu'elle soit facilement lisible. Nous pouvons utiliser la fonction `format` de la bibliothèque `Time` pour cela. Voyons un exemple concret :

```Elm
import Time

-- fonction pour calculer la date dans le futur
getDateFuture : Int -> String
getDateFuture days =
    let
        currentDate = Time.now
        futureDate = Time.add Time.day (days) currentDate
        formattedDate = Time.format "%d/%m/%Y" futureDate
    in
        formattedDate

-- fonction pour calculer la date dans le passé
getDatePast : Int -> String
getDatePast days =
    let
        currentDate = Time.now
        pastDate = Time.sub Time.day (days) currentDate
        formattedDate = Time.format "%d/%m/%Y" pastDate
    in
        formattedDate
```

Dans cet exemple, nous avons créé deux fonctions, `getDateFuture` et `getDatePast`, qui prennent toutes deux un entier représentant le nombre de jours à ajouter ou soustraire et renvoient la date correspondante dans un format facilement lisible.

Maintenant, pour utiliser ces fonctions dans notre code, nous pouvons simplement les appeler en leur donnant le nombre de jours en paramètre. Par exemple :

```Elm
getDateFuture 5  -- renvoie la date dans 5 jours
getDatePast 10  -- renvoie la date il y a 10 jours
```

Et voilà, nous avons maintenant la possibilité de calculer des dates dans le futur ou dans le passé en utilisant Elm !

## Plongée profonde

Comme mentionné précédemment, nous avons utilisé la fonction `add` et `sub` pour ajouter ou soustraire des jours à la date actuelle. Il est important de noter que nous pouvons également utiliser ces fonctions pour ajouter ou soustraire d'autres unités de temps telles que les heures, les minutes, etc. De plus, la fonction `format` nous permet de personnaliser le format de la date affichée en utilisant des symboles spécifiques. Par exemple, `%d` représente le jour, `%m` représente le mois et `%Y` représente l'année.

Il existe également d'autres fonctions utiles dans la bibliothèque `Time` qui peuvent être utilisées pour manipuler les dates et les heures en Elm. N'hésitez pas à explorer davantage et à en apprendre davantage sur ces fonctions.

## Voir aussi

- La documentation de la bibliothèque `Time` : [https://package.elm-lang.org/packages/elm/time/latest/](https://package.elm-lang.org/packages/elm/time/latest/) 
- L'article "Manipuler les dates et les heures en Elm" : [https://dev.to/salakar/manipulating-dates-and-times-in-elm-bi1](https://dev.to/salakar/manipulating-dates-and-times-in-elm-bi1)