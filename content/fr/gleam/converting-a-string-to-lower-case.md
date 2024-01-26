---
title:                "Conversion d'une chaîne de caractères en minuscules"
date:                  2024-01-20T17:38:21.745911-07:00
model:                 gpt-4-1106-preview
simple_title:         "Conversion d'une chaîne de caractères en minuscules"
programming_language: "Gleam"
category:             "Gleam"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fr/gleam/converting-a-string-to-lower-case.md"
---

{{< edit_this_page >}}

## What & Why? - Quoi et Pourquoi?
Convertir une chaîne de caractères en minuscules signifie transformer tous les caractères alphabétiques en leur équivalent en minuscule. Les programmeurs le font pour standardiser les entrées, faciliter les comparaisons et la recherche de données.

## How to: - Comment faire :
```gleam
import gleam/string

pub fn main() {
  let text = "Bonjour, Gleam!"
  let lowercased_text = string.to_lowercase(text)

  // Affiche le texte en minuscules
  io.debug(lowercased_text) // "bonjour, gleam!"
}
```

## Deep Dive - Plongée en profondeur
Convertisseur une chaîne de caractères en minuscules est une pratique courante remontant aux premiers jours de l'informatique. Historiquement, cela a facilité la mise en correspondance de chaînes de caractères et l'indexation, indépendamment de la casse, dans les bases de données et les recherche de texte.

Gleam fait cela avec la fonction `string.to_lowercase`, mais il y a des alternatives selon le langage : `.toLowerCase()` en JavaScript, `.lower()` en Python, etc. Le concept est universel.

L'implémentation est souvent basée sur les spécifications Unicode pour assurer la compatibilité avec divers alphabets et caractères spéciaux, ce qui peut ajouter une couche de complexité au traitement des chaînes de caractères.

## See Also - Voir également
- Unicode Case Mapping: [https://www.unicode.org/reports/tr21/](https://www.unicode.org/reports/tr21/)
- Rust String Methods (Inspirations for Gleam): [https://doc.rust-lang.org/std/string/struct.String.html#method.to_lowercase](https://doc.rust-lang.org/std/string/struct.String.html#method.to_lowercase)
