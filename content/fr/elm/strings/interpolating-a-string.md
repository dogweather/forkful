---
title:                "Interpolation de chaînes de caractères"
date:                  2024-01-20T17:50:33.031347-07:00
model:                 gpt-4-1106-preview
simple_title:         "Interpolation de chaînes de caractères"

tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fr/elm/interpolating-a-string.md"
---

{{< edit_this_page >}}

## Quoi & Pourquoi ?
L'interpolation de chaîne permet d'insérer des valeurs variables dans un texte fixe. Les programmeurs l'utilisent pour composer des messages personnalisés ou afficher des données dynamiques.

## Comment faire :
```elm
nom = "Amélie"
age = 28

message = "Bonjour, je m'appelle " ++ nom ++ " et j'ai " ++ String.fromInt(age) ++ " ans."

-- Sortie: "Bonjour, je m'appelle Amélie et j'ai 28 ans."
```

## Plongée en profondeur
Historiquement, Elm ne proposait pas d'interpolation de chaîne comme d'autres langages. Pour fusionner des chaînes et des valeurs, on utilise l'opérateur `++`. Une alternative est l'utilisation de la fonction `String.concat`, qui combine une liste de chaînes. L'implémentation repose sur une manipulation de chaînes standard, sans magie sous-jacente.

## Voir Aussi
- Elm documentation for `String`: https://package.elm-lang.org/packages/elm/core/latest/String
- Elm syntax guide on string operations: https://elm-lang.org/docs/syntax#strings
