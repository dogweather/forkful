---
aliases:
- /fr/haskell/rounding-numbers/
date: 2024-01-26 03:44:46.720954-07:00
description: "Arrondir des nombres signifie les ajuster au plus proche entier ou \xE0\
  \ la d\xE9cimale sp\xE9cifi\xE9e. Les programmeurs arrondissent les nombres pour\
  \ contr\xF4ler la\u2026"
lastmod: 2024-02-18 23:09:08.869469
model: gpt-4-0125-preview
summary: "Arrondir des nombres signifie les ajuster au plus proche entier ou \xE0\
  \ la d\xE9cimale sp\xE9cifi\xE9e. Les programmeurs arrondissent les nombres pour\
  \ contr\xF4ler la\u2026"
title: Arrondir les nombres
---

{{< edit_this_page >}}

## Quoi et Pourquoi ?

Arrondir des nombres signifie les ajuster au plus proche entier ou à la décimale spécifiée. Les programmeurs arrondissent les nombres pour contrôler la précision, adapter les sorties pour la présentation à l'utilisateur ou réduire les coûts de calcul pour les opérations en virgule flottante.

## Comment faire :

Haskell utilise les fonctions `round`, `ceiling`, `floor` et `truncate` du `Prelude` pour les opérations d'arrondi.

```haskell
import Prelude

main :: IO ()
main = do
  let num = 3.567
  print $ round num    -- 4
  print $ ceiling num  -- 4
  print $ floor num    -- 3
  print $ truncate num -- 3
  
  -- L'arrondi à une décimale spécifique n'est pas dans Prelude.
  -- Voici une fonction personnalisée :
  let roundTo n f = (fromInteger $ round $ f * (10^n)) / (10.0^^n)
  print $ roundTo 1 num -- 3.6
```

## Approfondissement

Historiquement, l'arrondi est significatif dans l'analyse numérique et l'informatique car il est crucial pour minimiser l'accumulation d'erreurs dans les calculs, particulièrement avant que les représentations à virgule flottante ne soient standardisées avec l'IEEE 754.

À quoi arrondir ? `round` vous amène au plus proche entier – vers le haut ou le bas. `ceiling` et `floor` arrondissent toujours au plus proche entier, respectivement vers le haut ou vers le bas, tandis que `truncate` se contente de supprimer les points décimaux.

Des alternatives à ces fonctions pourraient impliquer une logique personnalisée, comme notre `roundTo`, ou vous pourriez utiliser des bibliothèques (comme Data.Fixed) pour des besoins plus complexes.

Faites attention aux résultats inattendus dus à la manière dont Haskell gère les cas à mi-chemin dans `round` (il arrondit au nombre pair le plus proche).

## Voir Aussi

- Documentation de Prelude Haskell pour les fonctions d'arrondi : https://hackage.haskell.org/package/base-4.16.1.0/docs/Prelude.html
- Le Wiki Haskell sur l'arithmétique à virgule flottante : https://wiki.haskell.org/Floating_point_arithmetic
- La norme IEEE 754-2008 pour en savoir plus sur le traitement des nombres à virgule flottante dans de nombreux langages : https://ieeexplore.ieee.org/document/4610935
