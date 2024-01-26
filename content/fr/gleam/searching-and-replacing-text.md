---
title:                "Recherche et remplacement de texte"
date:                  2024-01-20T17:57:44.611243-07:00
model:                 gpt-4-1106-preview
simple_title:         "Recherche et remplacement de texte"
programming_language: "Gleam"
category:             "Gleam"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fr/gleam/searching-and-replacing-text.md"
---

{{< edit_this_page >}}

## What & Why? / Quoi et pourquoi ?
Chercher et remplacer du texte, c'est modifier une chaîne en trouvant une partie spécifique et en la substituant. Les programmeurs utilisent cette technique pour corriger des erreurs, mettre à jour des données ou automatiser des éditions de masse.

## How to / Comment faire :
```gleam
import gleam/string

pub fn main() {
  let text = "Bonjour Paris, la ville lumière!"
  let new_text = string.replace(text, "Paris", "Lyon")
  new_text
  // "Bonjour Lyon, la ville lumière!"
}
```

## Deep Dive / Plongée profonde
Remplacer du texte est un besoin de base en programmation. Dans les années 70, des outils comme `sed` (stream editor) pour Unix sont nés, permettant ces opérations via des lignes de commande. En Gleam, le module `string` fournit des fonctions pour manipuler des chaînes de caractères. Des langages comme Python ou JavaScript ont des fonctions similaires.

Alternative : l'utilisation d'expressions régulières pour des besoins plus complexes.

Détails d'implémentation : `string.replace` parcourt la chaîne source, trouve les occurrences et les remplace. La complexité dépend de l'algorithme utilisé par le langage.

## See Also / Voir aussi
- Unix `sed` command for stream editing: [https://www.gnu.org/software/sed/manual/sed.html](https://www.gnu.org/software/sed/manual/sed.html)
