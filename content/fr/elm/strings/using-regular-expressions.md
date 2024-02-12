---
title:                "Utilisation des expressions régulières"
aliases:
- /fr/elm/using-regular-expressions/
date:                  2024-02-03T19:16:35.476321-07:00
model:                 gpt-4-0125-preview
simple_title:         "Utilisation des expressions régulières"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fr/elm/using-regular-expressions.md"
changelog:
  - 2024-02-03, gpt-4-0125-preview, translated from English
---

{{< edit_this_page >}}

## Quoi & Pourquoi ?
Les expressions régulières (regex) en programmation sont des motifs utilisés pour la correspondance de combinaisons de caractères dans des chaînes de caractères. Dans Elm, comme dans d'autres langages, les programmeurs utilisent les regex pour des tâches telles que la validation d'entrée, la recherche et le remplacement de texte dans les chaînes en raison de leur flexibilité et de leur efficacité.

## Comment faire :
Elm n'a pas de fonctions regex intégrées dans sa bibliothèque principale, nécessitant l'utilisation de bibliothèques tierces pour ces opérations. Un des choix populaires pour travailler avec les regex est `elm/regex`. Vous pouvez l'ajouter à votre projet en utilisant `elm install elm/regex`.

Voici comment vous pouvez utiliser `elm/regex` pour quelques tâches courantes :

### 1. Correspondance d'un motif
Pour vérifier si une chaîne correspond à un motif, vous pouvez utiliser `Regex.contains`.

```elm
import Regex

pattern : Regex.Regex
pattern = Regex.fromString "^[a-zA-Z0-9]+$" |> Maybe.withDefault Regex.never

isAlphanumeric : String -> Bool
isAlphanumeric input = Regex.contains pattern input

-- Exemple d'utilisation :
isAlphanumeric "Elm2023"     -- Sortie : True
isAlphanumeric "Elm 2023!"   -- Sortie : False
```

### 2. Trouver toutes les correspondances
Pour trouver toutes les occurrences d'un motif au sein d'une chaîne, vous pouvez utiliser `Regex.find`.

```elm
matches : Regex.Regex
matches = Regex.fromString "\\b\\w+\\b" |> Maybe.withDefault Regex.never

getWords : String -> List String
getWords input = 
    input
        |> Regex.find matches
        |> List.map (.match)

-- Exemple d'utilisation :
getWords "Elm est amusant !"  -- Sortie : ["Elm", "est", "amusant"]
```

### 3. Remplacer du texte
Pour remplacer des parties d'une chaîne qui correspondent à un motif, vous utilisez `Regex.replace`.

```elm
replacePattern : Regex.Regex
replacePattern = Regex.fromString "Elm" |> Maybe.withDefault Regex.never

replaceElmWithHaskell : String -> String
replaceElmWithHaskell input = 
    Regex.replace replacePattern (\_ -> "Haskell") input

-- Exemple d'utilisation :
replaceElmWithHaskell "Apprendre Elm est amusant !"  
-- Sortie : "Apprendre Haskell est amusant !"
```

Dans ces exemples, `Regex.fromString` est utilisé pour compiler un motif regex, où `\b` correspond aux limites de mots, et `\w` correspond à tout caractère de mot. Gérez toujours le résultat `Maybe` de `Regex.fromString` pour vous protéger contre les motifs regex invalides, typiquement en utilisant `Maybe.withDefault`.
