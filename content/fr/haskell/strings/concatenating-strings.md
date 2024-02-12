---
title:                "Concaténation de chaînes de caractères"
aliases:
- /fr/haskell/concatenating-strings.md
date:                  2024-01-20T17:35:05.197028-07:00
model:                 gpt-4-1106-preview
simple_title:         "Concaténation de chaînes de caractères"

tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fr/haskell/concatenating-strings.md"
---

{{< edit_this_page >}}

## What & Why? (Quoi et Pourquoi?)
Concaténer des chaînes signifie les joindre bout à bout. Les programmeurs font cela pour assembler des textes, des messages, ou des données générés dynamiquement.

## How to: (Comment faire:)
```Haskell
-- Concaténation simple avec l'opérateur (++)
helloWorld = "Hello" ++ " " ++ "World!"
-- helloWorld vaut "Hello World!"

-- Concaténer avec la fonction concat et une liste de chaînes
sentenceList = concat ["Haskell", " ", "est", " ", "cool."]
-- sentenceList vaut "Haskell est cool."

-- Utiliser concatMap pour ajouter un espace après chaque mot sauf le dernier
sentenceSpace = concatMap (++" ") ["Haskell", "est", "vraiment", ""] ++ "cool."
-- sentenceSpace vaut "Haskell est vraiment cool."
```

## Deep Dive (Plongée Profonde)
Historiquement, concaténer des chaînes en Haskell n'est pas différent d'autres langages; ça reste une opération fondamentale. Haskell possède une approche fonctionnelle avec des opérateurs et des fonctions dédiées.

Alternatives:
- `(++)` est idéal pour de courtes concaténations.
- `concat` et `concatMap` sont efficaces avec des listes de chaînes.
- `intercalate` de Data.List ajoute une chaîne entre chaque élément d'une liste lors de la concaténation, très utile pour des motifs répétitifs.

Détails d'implémentation:
- Les chaînes en Haskell sont des listes de caractères, la concaténation revient donc à fusionner des listes, ce qui n'est pas le plus performant pour de longs textes.
- Pour de meilleures performances et manipulations, des bibliothèques comme `Text` de `Data.Text` (pour du texte mutable) ou `ByteString` sont souvent recommandées.

## See Also (Voir Aussi)
- Haskell `Data.Text` documentation: https://hackage.haskell.org/package/text
- Haskell `ByteString` documentation: https://hackage.haskell.org/package/bytestring
- Tutoriel sur les listes en Haskell (affinités avec les chaînes de caractères): https://www.learnhaskell.com/tutorial/haskell-lists-and-tuples
