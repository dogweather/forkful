---
changelog:
- 2024-02-03, gpt-4-0125-preview, translated from English
date: 2024-02-03 19:16:35.476321-07:00
description: "Comment faire : Elm n'a pas de fonctions regex int\xE9gr\xE9es dans\
  \ sa biblioth\xE8que principale, n\xE9cessitant l'utilisation de biblioth\xE8ques\
  \ tierces pour ces\u2026"
lastmod: '2024-03-13T22:44:57.678249-06:00'
model: gpt-4-0125-preview
summary: "Elm n'a pas de fonctions regex int\xE9gr\xE9es dans sa biblioth\xE8que principale,\
  \ n\xE9cessitant l'utilisation de biblioth\xE8ques tierces pour ces op\xE9rations."
title: "Utilisation des expressions r\xE9guli\xE8res"
weight: 11
---

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
