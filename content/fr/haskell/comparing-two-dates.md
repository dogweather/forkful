---
title:                "Comparaison de deux dates"
html_title:           "Haskell: Comparaison de deux dates"
simple_title:         "Comparaison de deux dates"
programming_language: "Haskell"
category:             "Haskell"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fr/haskell/comparing-two-dates.md"
---

{{< edit_this_page >}}

## Pourquoi

Si vous travaillez avec des données temporelles dans vos programmes, il est probable que vous ayez besoin de les comparer à un moment donné. La comparaison de deux dates peut être utile pour vérifier si elles sont égales, les trier ou les utiliser dans des conditions logiques.

## Comment faire

Pour comparer deux dates en Haskell, nous pouvons utiliser la fonction `compare` de la bibliothèque `Data.Time`. Cette fonction prend en paramètre deux valeurs de type `Day` (date) et renvoie un ordre de tri, représenté par le type de données `Ordering` (Ordre). Voici un exemple de code montrant comment utiliser `compare` pour trier une liste de dates:

```Haskell
import Data.Time

main = do
    let dates = [fromGregorian 2021 10 12, fromGregorian 2021 10 10, fromGregorian 2021 10 11]
    print (sortBy compare dates)
```

La sortie de ce code sera `[2021-10-10,2021-10-11,2021-10-12]`, car la fonction `compare` a trié les dates dans l'ordre croissant.

## Plongée en profondeur

Vous avez peut-être remarqué que la fonction `compare` renvoie le type `Ordering` plutôt que le résultat direct de la comparaison (par exemple, `True` ou `False`). Cela est dû au fait que la comparaison peut donner trois résultats: `LT` (Less Than- inférieur à), `GT` (Greater Than - supérieur à) ou `EQ` (Equal - égal). Cela peut sembler inutile, mais cela peut être utile si vous comparez des dates avec des horodatages.

En plus de `compare`, il existe d'autres fonctions utiles dans la bibliothèque `Data.Time` pour la comparaison de dates. Par exemple, `diffDays` renvoie le nombre de jours entre deux dates. Cela peut être utile pour trouver la différence entre deux événements ou pour calculer l'âge d'une personne à partir de sa date de naissance.

## Voir aussi

- Documentation officielle sur la bibliothèque `Data.Time` : https://hackage.haskell.org/package/time/docs/Data-Time.html
- Un tutoriel sur les dates en Haskell : https://mmhaskell.com/blog/2017/5/22/dates-and-times-in-haskell