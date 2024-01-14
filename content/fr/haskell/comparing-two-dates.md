---
title:                "Haskell: Comparaison de deux dates"
simple_title:         "Comparaison de deux dates"
programming_language: "Haskell"
category:             "Haskell"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fr/haskell/comparing-two-dates.md"
---

{{< edit_this_page >}}

## Pourquoi

La comparaison de deux dates est une tâche courante dans la programmation en Haskell. Elle permet de déterminer si une date est antérieure, égale ou postérieure à une autre date, ce qui peut être utile pour trier des données ou réaliser des calculs basés sur des dates.

## Comment faire

Pour comparer deux dates en Haskell, nous utiliserons le type de données `UTCTime` du module `Data.Time` ainsi que la fonction `compare` du module `Data.Ord`. Voici un exemple de code qui compare deux dates et affiche le résultat :

```Haskell
import Data.Time
import Data.Ord

-- Définition de deux dates
date1 = UTCTime (fromGregorian 2020 1 1) (secondsToDiffTime 0)
date2 = UTCTime (fromGregorian 2021 1 1) (secondsToDiffTime 0)

-- Comparaison de date1 et date2
comparison = compare date1 date2

-- Affichage du résultat
print comparison
```
Output: `LT`

Dans cet exemple, nous avons comparé la date du 1er janvier 2020 à celle du 1er janvier 2021. Comme le résultat est `LT` (pour "Lower Than"), cela signifie que date1 est antérieure à date2.

## Plongée plus profonde

Pour mieux comprendre comment la fonction `compare` fonctionne pour comparer des dates, il est important de comprendre comment les dates sont représentées en Haskell. Dans l'exemple ci-dessus, nous avons utilisé le type de données `UTCTime` qui représente une date et une heure en temps universel coordonné. Cette représentation est basée sur un nombre de secondes écoulées depuis l'époque Unix (1er janvier 1970 à minuit UTC).

Lorsque la fonction `compare` est appliquée à deux `UTCTime`, elle convertit d'abord ces valeurs en nombres de secondes, puis les compare. Ainsi, une date antérieure à une autre sera représentée par un nombre de secondes plus petit et sera considérée comme "inférieure". De même, une date postérieure sera représentée par un nombre de secondes plus grand et sera considérée comme "supérieure".

## Voir aussi

- [Documentation du module Data.Time](https://hackage.haskell.org/package/time/docs/Data-Time.html)
- [Documentation du module Data.Ord](https://hackage.haskell.org/package/base/docs/Data-Ord.html)