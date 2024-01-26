---
title:                "Suppression de caractères correspondant à un motif"
date:                  2024-01-20T17:42:13.824012-07:00
model:                 gpt-4-1106-preview
simple_title:         "Suppression de caractères correspondant à un motif"
programming_language: "Gleam"
category:             "Gleam"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fr/gleam/deleting-characters-matching-a-pattern.md"
---

{{< edit_this_page >}}

## What & Why? (Quoi et Pourquoi ?)

Supprimer des caractères selon un motif, c'est chercher des séquences spécifiques dans un texte et les enlever. Les programmeurs le font pour nettoyer les données, simplifier du texte, ou préparer des informations pour un traitement ultérieur.

## How to: (Comment faire :)

```gleam
import gleam/string

// Supprimer des chiffres d'une chaîne de caractères
fn remove_digits(text: String) -> String {
  string.replace(text, "[0-9]", "", case_sensitive: True)
}

fn main() {
  let clean_text = remove_digits("Voici 1 exemple avec 2 chiffres.")
  assert clean_text == "Voici  exemple avec  chiffres."
}
```

Sortie attendue :

```
"Voici  exemple avec  chiffres."
```

## Deep Dive (Plongée en profondeur)

Historiquement, la suppression de caractères suivant un motif est issue de l'utilisation d'expressions régulières, une méthode puissante pour manipuler du texte qui remonte aux années 1950. Dans Gleam, on utilise la fonction `replace` du module `string` pour ces opérations, qui offre une syntaxe plus moderne et sécurisée par rapport à d'autres langages. Son moteur utilise des motifs de correspondance, semblables aux expressions régulières, mais Gleam vise à être plus simple et éviter certains pièges des regex. Alternativement, il est possible de boucler manuellement sur une chaîne et de construire une nouvelle chaîne sans les caractères indésirables, mais cette méthode est moins succincte et souvent moins performante.

## See Also (Voir également)

- Expressions régulières et leur histoire : [https://en.wikipedia.org/wiki/Regular_expression](https://en.wikipedia.org/wiki/Regular_expression)
- Guide de démarrage rapide de Gleam : [https://gleam.run/book/tour/](https://gleam.run/book/tour/)
