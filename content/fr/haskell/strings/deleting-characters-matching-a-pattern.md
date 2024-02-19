---
aliases:
- /fr/haskell/deleting-characters-matching-a-pattern/
date: 2024-01-20 17:42:15.235094-07:00
description: "Supprimer des caract\xE8res correspondant \xE0 un motif consiste \xE0\
  \ enlever de notre texte des s\xE9quences sp\xE9cifiques de caract\xE8res. On le\
  \ fait souvent pour\u2026"
lastmod: 2024-02-18 23:09:08.857559
model: gpt-4-1106-preview
summary: "Supprimer des caract\xE8res correspondant \xE0 un motif consiste \xE0 enlever\
  \ de notre texte des s\xE9quences sp\xE9cifiques de caract\xE8res. On le fait souvent\
  \ pour\u2026"
title: "Suppression de caract\xE8res correspondant \xE0 un motif"
---

{{< edit_this_page >}}

## What & Why? (Quoi et Pourquoi ?)

Supprimer des caractères correspondant à un motif consiste à enlever de notre texte des séquences spécifiques de caractères. On le fait souvent pour nettoyer des données ou simplifier une chaîne de traitement.

## How to: (Comment faire :)

```haskell
import Data.List (delete)

-- Suppression simple
deleteChar :: Char -> String -> String
deleteChar _ "" = ""
deleteChar x xs = filter (/= x) xs

-- Utilisation de fonction 'deleteChar'
main :: IO ()
main = do
    let originalString = "Haskell est sympa!"
    let result = deleteChar 's' originalString
    putStrLn result  -- "Hakell et ympa!"
```

Un autre exemple en utilisant les expressions régulières pour plus de complexité :

```haskell
import Text.Regex.Posix ((=~))

-- Suppression avec motifs (expressions régulières)
deletePattern :: String -> String -> String
deletePattern _ "" = ""
deletePattern pattern text = text =~ ("[^" ++ pattern ++ "]+")

-- Utilisation de 'deletePattern'
main :: IO ()
main = do
    let originalString = "Offrez 0102 fleurs à 301"
    let cleanedString = deletePattern "0-9" originalString
    putStrLn cleanedString  -- "Offrez  fleurs à "
```

## Deep Dive (Plongée en Profondeur)

Historiquement, le traitement de chaînes de caractères a toujours été une part importante de la programmation. Dans Haskell, les chaînes sont des listes de caractères, donc la manipulation de chaîne s'appuie sur des fonctions puissantes pour les listes. En dehors de l'approche manuelle avec des fonctions comme `filter`, Haskell peut utiliser des bibliothèques comme `Text.Regex.Posix` pour les expressions régulières, ce qui offre un outil plus robuste pour des motifs complexes.

Alternativement, pour des buts plus spécifiques, on pourrait se tourner vers les parsers combinator library comme `Parsec` ou `Attoparsec`, qui fournissent une analyse approfondie et un contrôle des chaînes de caractères.

En termes de performance, Haskell est paresseux (lazy evaluation), ce qui signifie que les transformations de chaînes ne sont calculées que lorsque c'est absolument nécessaire. C'est utile pour les grandes données ou les streams.

## See Also (Voir Aussi)

- [Hackage: regex-posix](https://hackage.haskell.org/package/regex-posix)
- [Hackage: Parsec](https://hackage.haskell.org/package/parsec)
- [Hackage: Attoparsec](https://hackage.haskell.org/package/attoparsec)
