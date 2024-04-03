---
date: 2024-01-20 17:33:08.658032-07:00
description: "How to: Comparons deux dates en Haskell. On va utiliser `time` qui est\
  \ une biblioth\xE8que puissante pour manipuler les dates et les heures."
lastmod: '2024-03-13T22:44:57.846844-06:00'
model: gpt-4-1106-preview
summary: Comparons deux dates en Haskell.
title: Comparer deux dates
weight: 27
---

## How to:
Comparons deux dates en Haskell. On va utiliser `time` qui est une bibliothèque puissante pour manipuler les dates et les heures.

```Haskell
import Data.Time

-- Définissons deux dates
date1 = fromGregorian 2023 3 14  -- 14 mars 2023
date2 = fromGregorian 2021 6 18  -- 18 juin 2021

-- Comparons les dates
compareDates :: Day -> Day -> Ordering
compareDates = compare

-- Testons la fonction
main :: IO ()
main = do
    let result = compareDates date1 date2
    print result  -- Cela affichera GT parce que date1 est après date2
```

Sortie attendue :

```
GT
```

## Deep Dive
Comparer deux dates n'est pas nouveau. Dès qu'on a eu des calendriers, on a eu besoin de comparer les dates. En Haskell, `Data.Time` est le choix standard depuis la sortie de la version 1.0 de la bibliothèque en 2006. La fonction `compare` fournit une comparaison générique, mais pour les dates, `Data.Time` offre aussi des fonctions plus spécifiques comme `diffDays` pour obtenir le nombre de jours entre deux dates. Les alternatives incluent l'utilisation de bibliothèques tierces comme `thyme`, mais `time` reste le choix le plus robuste et le plus intégré dans l'écosystème Haskell.

## See Also
- Une bonne documentation sur `Data.Time`: http://hackage.haskell.org/package/time-1.9.3/docs/Data-Time.html
- Guide sur les types de date et d'heure en Haskell: https://www.stephendiehl.com/posts/haskell_2017.html#data.time
- Pour comprendre en profondeur la manipulation de date et heure : https://en.wikibooks.org/wiki/Haskell/Understanding_monads/IO
