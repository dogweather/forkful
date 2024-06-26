---
date: 2024-01-20 17:45:24.269969-07:00
description: "How to: En Elm, on utilise les fonctions `String.slice` pour d\xE9couper\
  \ une sous-cha\xEEne. Voici comment."
lastmod: '2024-03-13T22:44:57.676970-06:00'
model: gpt-4-1106-preview
summary: "En Elm, on utilise les fonctions `String.slice` pour d\xE9couper une sous-cha\xEE\
  ne."
title: "Extraction de sous-cha\xEEnes"
weight: 6
---

## How to:
En Elm, on utilise les fonctions `String.slice` pour découper une sous-chaîne. Voici comment:

```Elm
import String exposing (slice)

-- Extrait "Monde" de "Bonjour Monde"
substring : String -> String
substring str =
  slice 8 13 str

-- Exemple d'utilisation
main =
  String.fromList (substring "Bonjour Monde") -- "Monde"
```

## Deep Dive
Historiquement, l'extraction de sous-chaînes est un outil fondamental en programmation. En Elm, `String.slice` est préféré parce qu'il gère les unicode correctement contrairement à d'autres langages où ça peut être compliqué. Il existe aussi `String.left` et `String.right` pour obtenir des sous-chaînes depuis le début ou la fin.

## See Also
- Documentation Elm sur les chaînes de caractères: [Elm String](https://package.elm-lang.org/packages/elm/core/latest/String)
- Bonnes pratiques sur la manipulation de chaînes: [Practical Elm](https://elmprogramming.com/)
