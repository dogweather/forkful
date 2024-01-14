---
title:                "Haskell: Obtenir la date actuelle"
simple_title:         "Obtenir la date actuelle"
programming_language: "Haskell"
category:             "Haskell"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fr/haskell/getting-the-current-date.md"
---

{{< edit_this_page >}}

## Pourquoi 

Pourquoi devriez-vous apprendre à obtenir la date actuelle en Haskell ? En tant que langage de programmation fonctionnel, Haskell valorise l'utilisation de fonctions pures, ce qui signifie que les fonctions ne dépendent pas de l'état externe pour produire des résultats cohérents. Cela en fait un excellent choix pour manipuler les dates, car les données temporelles sont hautement dépendantes du contexte et peuvent être difficiles à gérer dans d'autres langages.

## Comment le faire

Pour obtenir la date actuelle en Haskell, nous allons utiliser la fonction `getCurrentTime` de la bibliothèque `Data.Time`. Cette fonction renvoie un objet `UTCTime` qui représente l'heure et la date universelles coordonnées (UTC).

```Haskell
import Data.Time

main = do
  currentTime <- getCurrentTime
  print currentTime
```

La sortie de ce code sera quelque chose comme `2021-11-11 09:35:00 UTC`. Cela peut être difficile à lire, mais il s'agit en fait d'un objet complexe contenant différentes informations sur l'heure et la date. Pour extraire des informations spécifiques, nous pouvons utiliser les fonctions telles que `utctDay` pour obtenir le jour de la date ou `utctDayTime` pour obtenir l'heure en secondes depuis minuit.

## Approfondissement

Il est important de comprendre que `getCurrentTime` renvoie l'heure et la date actuelles en fonction du système d'exploitation sur lequel le code est exécuté. Cela signifie que si vous exécutez le code sur différents ordinateurs ou à différents moments, vous obtiendrez des résultats différents. Il est également utile de noter qu'il existe d'autres types de données pour représenter les heures et les dates dans Haskell, tels que `ZonedTime` qui prend en compte les fuseaux horaires.

## Voir aussi 

- [Documentation officielle de `Data.Time`](https://hackage.haskell.org/package/time/docs/Data-Time.html)
- [Tutoriel sur les dates et heures en Haskell](https://mmhaskell.com/blog/2017/3/6/manipulating-dates-and-times-in-haskell)
- [Exemples pratiques d'utilisation de la bibliothèque `Data.Time`](https://wiki.haskell.org/Cookbook/Date_and_time)