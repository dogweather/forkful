---
title:                "Obtenir la date actuelle"
date:                  2024-02-03T19:09:31.312136-07:00
model:                 gpt-4-0125-preview
simple_title:         "Obtenir la date actuelle"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fr/haskell/getting-the-current-date.md"
changelog:
  - 2024-02-03, gpt-4-0125-preview, translated from English
---

{{< edit_this_page >}}

## Quoi & Pourquoi ?
Récupérer la date actuelle en Haskell implique d'obtenir le temps système actuel et de le transformer en un format de date lisible. Les programmeurs font cela pour effectuer des opérations basées sur la date, telles que la journalisation, la planification de tâches ou le marquage temporel d'événements dans les applications.

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
