---
title:                "Utiliser les expressions régulières"
html_title:           "C: Utiliser les expressions régulières"
simple_title:         "Utiliser les expressions régulières"
programming_language: "Gleam"
category:             "Gleam"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fr/gleam/using-regular-expressions.md"
---

{{< edit_this_page >}}

## C'est quoi et pourquoi?

Les expressions régulières (regular expressions) sont des séquences de caractères utilisées pour définir des modèles de recherche dans des chaînes de caractères. Les programmeurs les utilisent pour la manipulation de chaînes de caractères, le filtrage de données, et d'autres tâches de traitement du texte.

## Comment faire :

Voici comment utiliser les expressions régulières dans Gleam.

```gleam
import gleam/regex

let phrase = "Salut tout le monde!"
let pattern = regex.from_string("tout").unwrap() // créer un motif à partir d'une chaîne

case regex.find(pattern, phrase) {
  Ok(match) ->
    io.println(match) // Output: ["tout"]
  Error(_) ->
    io.println("Rien trouvé")
}
```

## Plongée en profondeur :

Historiquement, les expressions régulières sont apparues dans le langage Perl, mais ont depuis été adoptées par de nombreux autres langages de programmation. Dans Gleam, les expressions régulières sont supportées par le module `gleam/regex`.

Les alternatives aux expressions régulières incluent la recherche simple de chaînes et le parsing de chaînes, bien que ces méthodes soient généralement moins puissantes et flexibles.

Quant à l'implémentation, Gleam utilise la bibliothèque Erlang pour fournir une interface d'expressions régulières à la fois riche et performante.

## Voir aussi :

Pour plus d'informations, consultez les sources suivantes :

2. RegexOne, un excellent tutoriel interactif pour apprendre les expressions régulières : [RegexOne](https://regexone.com/)
3. Erlang Regular Expressions: [Erlang Regex](https://erlang.org/doc/man/re.html)