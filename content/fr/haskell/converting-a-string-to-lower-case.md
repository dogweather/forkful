---
title:                "Conversion d'une chaîne de caractères en minuscules"
date:                  2024-01-20T17:38:38.271900-07:00
model:                 gpt-4-1106-preview
simple_title:         "Conversion d'une chaîne de caractères en minuscules"
programming_language: "Haskell"
category:             "Haskell"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fr/haskell/converting-a-string-to-lower-case.md"
---

{{< edit_this_page >}}

## What & Why?
Convertir une chaîne de caractères en minuscules, c'est transformer tous les caractères alphabétiques en leurs équivalents en minuscule. Les programmeurs font ça pour normaliser les données, par exemple pour comparer des chaînes de manière insensible à la casse.

## How to:
Haskell rend ce processus assez simple avec la fonction `toLower` du module `Data.Char`. Voici comment ça marche :

```Haskell
import Data.Char (toLower)

-- Convertit tous les caractères d'une chaîne en minuscules
lowercaseString :: String -> String
lowercaseString = map toLower

-- Exemple d'utilisation
main :: IO ()
main = putStrLn $ lowercaseString "CeCI eST uN TeST!"
```

Sortie attendue:

```
ceci est un test!
```

## Deep Dive
La fonction `toLower` existe depuis les premières versions de Haskell, témoignant de l'importance de la manipulation de textes. L'alternative est d'écrire une fonction personnalisée qui gère manuellement les correspondances de casse, ce qui n'est ni élégant ni efficace.

Pourquoi utiliser `map toLower`? Haskell est fonctionnel et traite les chaînes comme des listes de caractères. La fonction `map` applique `toLower` à chaque élément de la liste, nous offrant une solution élégante et concise.

Les implémentations internes de `toLower` prennent en compte les spécificités des caractères Unicode, ce qui la rend robuste pour les textes internationaux.

## See Also
- Haskell `Data.Char` documentation: https://hackage.haskell.org/package/base-4.16.0.0/docs/Data-Char.html
- Article on "Text manipulation in Haskell": https://www.schoolofhaskell.com/school/to-infinity-and-beyond/older-but-still-useful/text-manipulation-in-haskell
- Tutorial "Learn You a Haskell for Great Good!": http://learnyouahaskell.com/chapters
