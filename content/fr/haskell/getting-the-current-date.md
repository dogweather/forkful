---
title:                "Obtenir la date actuelle"
date:                  2024-01-20T15:14:41.973384-07:00
simple_title:         "Obtenir la date actuelle"

tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fr/haskell/getting-the-current-date.md"
---

{{< edit_this_page >}}

## What & Why?
Obtenir la date actuelle en Haskell est fondamental. Les programmeurs le font pour des logs, des timestamps ou des fonctionnalités dépendantes du temps.

## How to:
Pour choper la date du jour, Haskell est ton ami. Utilise `Data.Time` ; check l'exemple :

```Haskell
import Data.Time

main :: IO ()
main = do
    currentDay <- getCurrentTime
    putStrLn $ "La date et l'heure actuelles sont : " ++ show currentDay
```

Si tu lances ça, voici ce que tu auras :

```
La date et l'heure actuelles sont : 2023-04-14 12:34:56.7891011 UTC
```

Simple, non ?

## Deep Dive
Haskell et les dates, c'est une vieille histoire. `Data.Time` est l'évolution moderne, plus simple d'usage que l'antique `System.Time`. Côté alternatives, tu peux tenter `old-time` mais franchement, `Data.Time` t'offre tout ce qu'il te faut. Pense à `TimeZone` et `FormatTime` pour des manip' plus complexes (comme gérer les fuseaux horaires). Haskell gère les dates système via les types de données comme `UTCTime`, et tu peux formatter à ta sauce avec le package `time`.

## See Also
Pour creuser :

- [`Data.Time` documentation](https://hackage.haskell.org/package/time-1.9.3/docs/Data-Time.html)
- [Time package sur Hackage](https://hackage.haskell.org/package/time)
