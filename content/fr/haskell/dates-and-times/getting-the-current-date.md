---
changelog:
- 2024-02-03, gpt-4-0125-preview, translated from English
date: 2024-02-03 19:09:31.312136-07:00
description: "R\xE9cup\xE9rer la date actuelle en Haskell implique d'obtenir le temps\
  \ syst\xE8me actuel et de le transformer en un format de date lisible. Les programmeurs\
  \ font\u2026"
lastmod: '2024-03-13T22:44:57.844871-06:00'
model: gpt-4-0125-preview
summary: "R\xE9cup\xE9rer la date actuelle en Haskell implique d'obtenir le temps\
  \ syst\xE8me actuel et de le transformer en un format de date lisible."
title: Obtenir la date actuelle
weight: 29
---

## Comment faire :
La bibliothèque standard de Haskell, `base`, fournit le module `Data.Time` qui offre des fonctionnalités pour travailler avec les dates et les heures. Voici comment l'utiliser pour obtenir la date actuelle :

```haskell
import Data.Time (getCurrentTime, utctDay)

main :: IO ()
main = do
    now <- getCurrentTime
    let today = utctDay now
    print today
```

Exemple de sortie :
```
2023-04-12
```

Pour plus de flexibilité, comme pour formater la date ou travailler avec différents fuseaux horaires, la bibliothèque `time` est inestimable. Voici comment vous pourriez formater la date actuelle :

```haskell
import Data.Time

main :: IO ()
main = do
    now <- getCurrentTime
    timezone <- getCurrentTimeZone
    let zoneNow = utcToLocalTime timezone now
    putStrLn $ formatTime defaultTimeLocale "%Y-%m-%d" zoneNow
```

Cela affiche la date actuelle au format `AAAA-MM-JJ`, ajusté au fuseau horaire local.

De plus, pour le support de bibliothèques tierces, `time` est fortement recommandé et souvent utilisé au sein de la communauté Haskell pour ses capacités étendues de manipulation des dates et des heures. Les exemples ci-dessus utilisent cette bibliothèque.

Si vous avez besoin de manipulations de dates plus complètes, y compris le parsing à partir de chaînes ou des opérations arithmétiques avec des dates et des heures, explorer les fonctions supplémentaires dans `Data.Time` sera bénéfique.
