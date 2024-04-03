---
date: 2024-01-20 17:50:33.031347-07:00
description: 'Comment faire : .'
lastmod: '2024-03-13T22:44:57.673927-06:00'
model: gpt-4-1106-preview
summary: .
title: "Interpolation de cha\xEEnes de caract\xE8res"
weight: 8
---

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
