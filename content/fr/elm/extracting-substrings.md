---
title:                "Extraction de sous-chaînes"
date:                  2024-01-20T17:45:24.269969-07:00
model:                 gpt-4-1106-preview
simple_title:         "Extraction de sous-chaînes"
programming_language: "Elm"
category:             "Elm"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fr/elm/extracting-substrings.md"
---

{{< edit_this_page >}}

## What & Why?
Extraire des sous-chaînes, c'est récupérer des parties spécifiques d'une chaîne de caractères. On le fait pour analyser, formater ou manipuler des données textuelles.

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
