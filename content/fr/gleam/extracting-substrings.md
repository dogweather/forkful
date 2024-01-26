---
title:                "Extraction de sous-chaînes"
date:                  2024-01-20T17:45:27.310911-07:00
model:                 gpt-4-1106-preview
simple_title:         "Extraction de sous-chaînes"
programming_language: "Gleam"
category:             "Gleam"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fr/gleam/extracting-substrings.md"
---

{{< edit_this_page >}}

## What & Why? (Quoi et Pourquoi ?)
Extraire des sous-chaînes, c'est récupérer des parties spécifiques d'une chaîne de caractères. On le fait pour analyser, transformer, ou valider des données textuelles.

## How to: (Comment faire :)
```gleam
import gleam/string

pub fn main() {
  let text = "Bonjour, monde!"
  let hello = string.slice(text, 0, 7) // On récupère "Bonjour"
  let world = string.slice(text, 9, 14) // Et puis "monde"
  io.debug(hello) // Affiche "Bonjour"
  io.debug(world) // Affiche "monde"
}
```

## Deep Dive (Plongée Profonde)
L'extraction de sous-chaînes existe depuis les premiers langages de programmation pour manipuler les données textuelles. En Gleam, elle est sûre et prévoit les erreurs de dépassement grâce au typage statique fort. À côté, les langages comme Python ou JavaScript offrent des fonctions similaires mais avec des comportements différents en cas d'erreurs, comme des exceptions ou des résultats inattendus. Gleam, avec sa conception délibérée, évite ces pièges potentiels.

## See Also (Voir aussi)
- Documentation Gleam sur les chaînes: https://hexdocs.pm/gleam_stdlib/gleam/string/
- Tutoriel Gleam: https://gleam.run/book/tour/
- Forum de Gleam pour poser vos questions: https://github.com/gleam-lang/gleam/discussions
