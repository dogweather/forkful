---
title:                "Conversion d'une date en chaîne de caractères"
date:                  2024-01-20T17:36:43.360644-07:00
model:                 gpt-4-1106-preview
simple_title:         "Conversion d'une date en chaîne de caractères"

tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fr/haskell/converting-a-date-into-a-string.md"
---

{{< edit_this_page >}}

## Quoi & Pourquoi ?
Convertir une date en chaîne nous permet de la formater pour l'affichage. Les programmeurs le font pour rendre les dates compréhensibles par les humains ou préparer pour la sérialisation.

## Comment faire :
```Haskell
import Data.Time

-- Convertit une date en une chaîne avec un formatage standard
dateToString :: IO String
dateToString = do
    current_time <- getCurrentTime
    let date = utctDay current_time
    return $ showGregorian date

main :: IO ()
main = do
    dateString <- dateToString
    putStrLn dateString
```
Sortie possible :
```
2023-03-15
```

## Plongeon en profondeur
Haskell utilise le package `time` pour gérer les dates. Historiquement, `Data.Time` est le module standard pour travailler avec le temps et les dates, succédant à des bibliothèques moins complètes. Alternativement, on peut utiliser `formatTime` pour un contrôle précis sur le format de sortie. L'implémentation repose sur des types abstraits comme `UTCTime`, ce qui assure la validité des données.

## Voir aussi
- [Haskell `time` package](https://hackage.haskell.org/package/time)
- [`Data.Time.Format` documentation](https://hackage.haskell.org/package/time-1.9.3/docs/Data-Time-Format.html)
- [Haskell Hierarchical Libraries](https://downloads.haskell.org/~ghc/latest/docs/html/libraries/)
