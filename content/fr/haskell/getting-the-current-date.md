---
title:                "Obtenir la date actuelle"
html_title:           "Haskell: Obtenir la date actuelle"
simple_title:         "Obtenir la date actuelle"
programming_language: "Haskell"
category:             "Haskell"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fr/haskell/getting-the-current-date.md"
---

{{< edit_this_page >}}

## Pourquoi

Si vous créez une application ou un programme qui nécessite d'afficher la date actuelle, il est utile de savoir comment obtenir cette information dynamiquement. Cela permet de garder les informations à jour et d'ajouter des fonctionnalités liées au temps, telles que des rappels ou des tâches planifiées.

## Comment faire

Dans Haskell, il existe plusieurs façons d'obtenir la date actuelle en utilisant le module "Data.Time". Voici un exemple de code montrant comment obtenir la date actuelle et l'afficher dans le format "jour/mois/année":

```Haskell
import Data.Time
main = do
  currentTime <- getCurrentTime
  let (year, month, day) = toGregorian $ utctDay currentTime
  putStrLn $ show day ++ "/" ++ show month ++ "/" ++ show year
```

Si vous souhaitez afficher la date et l'heure, vous pouvez utiliser la fonction "getCurrentTime" et la fonction "getCurrentTimezone" pour obtenir la zone horaire actuelle. Voici un exemple de code montrant comment afficher la date et l'heure actuelles dans le format "jour/mois/année heure:minute:seconde":

```Haskell
import Data.Time
main = do
  currentTime <- getCurrentTime
  currentTimezone <- getCurrentTimeZone
  let (year, month, day) = toGregorian $ utctDay currentTime
  let (TimeOfDay hour minute second) = timeToTimeOfDay $ utctDayTime currentTime
  putStrLn $ show day ++ "/" ++ show month ++ "/" ++ show year ++ " " ++ show hour ++ ":" ++ show minute ++ ":" ++ show second ++ " " ++ show currentTimezone
```

Voici un exemple de sortie de ces deux codes:

```
1/10/2021
1/10/2021 16:30:12 CEST
```

## Plongée plus profonde

Vous vous demandez peut-être comment la fonction "getCurrentTime" obtient la date et l'heure actuelles. En fait, elle utilise une horloge système interne pour obtenir l'heure actuelle et la convertit ensuite en un type de données appelé "UTCTime" (temps universel coordonné). Ensuite, elle peut être manipulée et affichée dans différents formats, comme nous l'avons vu dans les exemples ci-dessus.

Il est également intéressant de noter que le module "Data.Time" contient d'autres fonctions utiles pour manipuler les dates et les heures, telles que "addUTCTime" pour ajouter une durée à un "UTCTime" et "diffDays" pour calculer la différence en jours entre deux "UTCTime".

## Voir également

- [Documentation officielle de Data.Time](https://downloads.haskell.org/~ghc/latest/docs/html/libraries/time-1.9.1/index.html)
- [Tutoriel Haskell sur les dates et les heures](https://www.tutorialspoint.com/haskell/haskell_date_time.htm)