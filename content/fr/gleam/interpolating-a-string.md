---
title:                "Interpolation de chaînes de caractères"
date:                  2024-01-20T17:50:38.244274-07:00
model:                 gpt-4-1106-preview
simple_title:         "Interpolation de chaînes de caractères"
programming_language: "Gleam"
category:             "Gleam"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fr/gleam/interpolating-a-string.md"
---

{{< edit_this_page >}}

## What & Why? (Quoi et Pourquoi ?)
L'interpolation de chaîne permet d'insérer des variables ou des expressions dans une chaîne de caractères. Ça simplifie la concaténation et rend le code plus lisible.

## How to: (Comment faire :)
```gleam
let name = "Monde"
let greeting = "Bonjour, {name}!"
println(greeting)
```
Sortie : `Bonjour, Monde!`

## Deep Dive (Plongée en profondeur)
Historiquement, l'interpolation de chaînes existe dans de nombreux langages et est pratique pour construire des messages dynamiques. En Gleam, elle est intégrée de manière élégante et sûre. Contrairement à d'autres langages où l'interpolation peut introduire des failles de sécurité, comme les injections SQL, Gleam maintient la sûreté du typage. Des alternatives comme la concaténation manuelle (`"Bonjour, " ++ name ++ "!"`) sont disponibles mais entraînent du code plus verbeux et une lisibilité réduite.

## See Also (Voir aussi)
- Documentation officielle de Gleam sur les chaînes de caractères : [Gleam Strings](https://gleam.run/book/tour/strings.html)