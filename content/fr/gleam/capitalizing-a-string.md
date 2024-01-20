---
title:                "Mettre une chaîne de caractères en majuscules"
html_title:           "Gleam: Mettre une chaîne de caractères en majuscules"
simple_title:         "Mettre une chaîne de caractères en majuscules"
programming_language: "Gleam"
category:             "Gleam"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fr/gleam/capitalizing-a-string.md"
---

{{< edit_this_page >}}

## Quoi & Pourquoi?
Capitaliser une chaîne signifie transformer la première lettre de chaque mot de la chaîne en majuscule. Les programmeurs font cela pour améliorer la lisibilité et rendre les données plus présentables aux utilisateurs.

## Comment faire:
Voici un exemple simple dans Gleam pour capitaliser une chaîne.

```Gleam
import gleam/string

fn capitalise_example() {
  let example_string = "bonjour, le monde!"
  let capitalised_string = string.capitalise(example_string)
  capitalised_string
}

fn main(_) {
  capitalise_example()
}
```

Dans cet exemple, si vous exécutez `capitalise_example()`, vous obtiendrez `"Bonjour, le monde!"` en sortie.

## Plongée en profondeur
Historiquement, la capitalisation a commencé avec l'écriture manuscrite, où l'utilisation des majuscules servait à mettre l'accent sur certains mots. En programmation, c'est un moyen de traiter les chaînes pour les présenter de manière plus formelle ou pour respecter certaines conventions syntaxiques.

Il existe plusieurs alternatives à `string.capitalise()` dans Gleam, comme `string.uppercase()` ou `string.lowercase()` pour rendre toute la chaîne en majuscules ou en minuscules, respectivement. Cependant, la fonction `capitalise()` est généralement préférée pour capitaliser proprement une phrase ou un titre.

Derrière les coulisses, la fonction `capitalise()` de Gleam examine chaque mot dans la chaîne, transforme la première lettre en majuscule et laisse les autres lettres en minuscules. Elle fait ceci en parcourant la chaîne une fois, ce qui rend cette fonction très efficace.

## Voir aussi
Pour plus de détails sur les fonctions de chaîne en Gleam, consultez la documentation officielle sur [Gleam/string](https://gleam.run/stdlib/string/). Vous pouvez également trouver des exemples de code supplémentaires et des explications détaillées sur le traitement des chaînes dans le guide d'apprentissage Gleam, disponible [ici](https://gleam.run/learning/).