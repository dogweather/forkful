---
title:                "Utilisation des expressions régulières"
html_title:           "Bash: Utilisation des expressions régulières"
simple_title:         "Utilisation des expressions régulières"
programming_language: "Gleam"
category:             "Gleam"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fr/gleam/using-regular-expressions.md"
---

{{< edit_this_page >}}

## What & Why? (Quoi et Pourquoi ?)
Les expressions régulières permettent de chercher et manipuler du texte en suivant des patterns. Les programmeurs les utilisent pour la puissance et la flexibilité qu'elles offrent dans le traitement des chaînes de caractères.

## How to: (Comment faire : )
Le code Gleam pour utiliser les expressions régulières est simple. Voici un exemple :

```gleam
import gleam/regex

fn main() {
  let pattern = regex.from_string("Hello (\\w+)!").unwrap()
  let result = regex.find(pattern, "Hello World!")
  result
}
```

Sortie attendue:

```
[Match(data="Hello World!", start=0, end=12)]
```

## Deep Dive (Plongée en Profondeur)
Les expressions régulières tirent leurs origines des théories mathématiques des années 1950. Des alternatives, comme le parsing structuré, existent mais sont souvent plus complexes pour des tâches simples. En Gleam, l'utilisation des expressions régulières est rendue plus sûre grâce à la gestion d'erreur de la fonction `from_string`.

## See Also (Voir Aussi)
- Documentation Gleam sur les Regex: [Gleam Regex Documentation](https://hexdocs.pm/gleam_stdlib/gleam/regex/)
- Tutoriel interactif Regex: [RegexOne](https://regexone.com/)
- 'A Play on Regular Expressions' pour comprendre les expressions régulières en s'amusant: [Regex Crossword](https://regexcrossword.com/)