---
title:                "Mettre une chaîne de caractères en majuscules"
date:                  2024-01-19
html_title:           "C: Mettre une chaîne de caractères en majuscules"
simple_title:         "Mettre une chaîne de caractères en majuscules"
programming_language: "Gleam"
category:             "Gleam"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fr/gleam/capitalizing-a-string.md"
---

{{< edit_this_page >}}

# Capitaliser des chaînes en Gleam

## What & Why? (Quoi & Pourquoi ?)
Capitaliser une chaîne, c'est transformer la première lettre en majuscule. Les programmeurs utilisent cette technique pour normaliser des données ou pour des aspects esthétiques, comme les titres ou les noms propres.

## How to: (Comment faire :)
```gleam
import gleam/string

pub fn main() {
  let example = "bonjour le monde!"
  let capitalized = string.capitalize(example)
  io.println(capitalized) // Output: "Bonjour le monde!"
}
```

## Deep Dive (Plongée profonde)
Historiquement, capitaliser était crucial pour les anciens manuscrits où les majuscules signalaient le début d'un texte important. En programmation, cela aide à uniformiser les entrées utilisateur, surtout quand la casse (majuscule/minuscule) varie. Alternativement, on peut tout mettre en majuscules ou en minuscules, mais c'est moins subtil. Techniquement, Gleam utilise les propriétés Unicode pour déterminer les transformations, ce qui assure une bonne prise en charge des diverses langues.

## See Also (Voir aussi)
- Unicode standard for text processing: [https://unicode.org/reports/tr15/](https://unicode.org/reports/tr15/)
- Practical use cases for text capitalization: [https://developer.mozilla.org/en-US/docs/Web/CSS/text-transform](https://developer.mozilla.org/en-US/docs/Web/CSS/text-transform)
