---
title:                "Haskell: Comparer deux dates"
programming_language: "Haskell"
category:             "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fr/haskell/comparing-two-dates.md"
---

{{< edit_this_page >}}

## Pourquoi

Comparaison de dates est une tâche courante en programmation, en particulier dans le développement d'applications liées au temps, telles que les calendriers ou les tâches planifiées. Comprendre comment comparer deux dates est un outil utile pour créer des fonctionnalités précises et fiables.

## Comment faire

Pour comparer deux dates en Haskell, nous avons besoin d'utiliser la fonction `compare` du module `Data.Time`. Prenons par exemple les deux dates suivantes :

```
date1 = fromGregorian 2021 10 01 -- 1er octobre 2021
date2 = fromGregorian 2021 09 25 -- 25 septembre 2021
```

La fonction `compare` prend deux arguments et renvoie un type de données `Ordering` qui peut être soit `GT` (plus grand), `LT` (plus petit) ou `EQ` (égal). Nous pouvons l'utiliser pour comparer les deux dates de la manière suivante :

```
compare date1 date2 -- LT (plus petit)
```

Il est également possible de comparer les dates dans un ordre inverse en utilisant la fonction `flip` :

```
flip compare date1 date2 -- GT (plus grand)
```

Nous pouvons également comparer les dates dans un contexte de tri en utilisant la fonction `sort` du module `Data.List` :

```
dates = [date1, date2]
sort dates -- [25 septembre 2021, 1er octobre 2021]
```

## Plongée en profondeur

La fonction `compare` utilise le fait que les dates peuvent être représentées comme des nombres entiers, en considérant le jour, le mois et l'année comme des chiffres. Cela permet d'obtenir le bon ordre des dates en les comparant directement.

Cependant, pour les dates avec des heures, minutes ou secondes, la fonction `compare` ne suffit pas car elle ne prend en compte que la date elle-même. Dans ce cas, il est nécessaire d'utiliser la fonction `compareUTCTime` du module `Data.Ord` qui prend en compte les heures.

## Voir aussi

- [Documentation de la fonction `compare` dans le module `Data.Time`](https://hackage.haskell.org/package/time-1.9.3/docs/Data-Time.html#t:Day)
- [Documentation de la fonction `compareUTCTime` dans le module `Data.Ord`](https://hackage.haskell.org/package/base-4.16.0.0/docs/Data-Ord.html#v:compareUTCTime)