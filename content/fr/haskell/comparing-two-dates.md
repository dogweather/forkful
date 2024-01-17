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

## Quoi & Pourquoi?
Comparer deux dates est une opération courante en programmation, qui consiste à vérifier si une date est antérieure, égale ou postérieure à une autre date. Les programmeurs font cela pour gérer des événements temporels ou pour trier des données par date.

## Comment faire:
Voici un exemple de comparaison de deux dates en Haskell:
```Haskell
compareDates :: Ord a => a -> a -> Ordering
compareDates date1 date2 = compare date1 date2
```
Sur lequel we peut appeler en utilisant deux dates de type ```Day``` dans le module ```Data. Time```:
```Haskell
compareDates (fromGregorian 2021 1 1) (fromGregorian 2020 1 1)
```
Ce qui retourne la valeur ```GT``` pour "greater than" (supérieure à).

## Deep Dive:
Dans le passé, comparer deux dates pouvait être compliqué en raison de différents systèmes de calendrier et de représentations de temps. Aujourd'hui, la plupart des langages de programmation, dont Haskell, utilisent le modèle de temps Unix pour simplifier ces opérations.

Une alternative à l'utilisation de la fonction ```compare``` est d'utiliser des opérateurs de comparaison tels que ```<```, ```>```, ```==```, etc. Il est important de noter que ces opérateurs peuvent être surchargés pour différents types de données, donc en utilisant ```compare``` on garantit la comparaison de dates au lieu d'une comparaison surchargée.

Pour implémenter la comparaison de dates en utilisant ```compare```, Haskell se base sur la notation de jours de calendrier proleptique, où les dates avant la réforme grégorienne sont comptées à rebours à partir de leur origine en -4713. La fonction ```fromGregorian``` dans le module ```Data.Time.Calendar``` fonctionne selon ce modèle.

## See Also:
Pour plus d'informations sur les opérations temporelles en Haskell, consultez la documentation du module ```Data.Time``` sur [Hackage](https://hackage.haskell.org/package/time-1.9.3/docs/Data-Time.html).

Pour des informations sur la notation de jours de calendrier proleptique, consultez [Wikipedia](https://fr.wikipedia.org/wiki/Calendrier_julien).