---
title:                "Analyse syntaxique de HTML"
date:                  2024-01-20T15:31:34.562990-07:00
html_title:           "Arduino: Analyse syntaxique de HTML"
simple_title:         "Analyse syntaxique de HTML"
programming_language: "Gleam"
category:             "Gleam"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fr/gleam/parsing-html.md"
---

{{< edit_this_page >}}

## What & Why? - Quoi & Pourquoi ?

Le parsing HTML, c'est transformer du code HTML en une structure qu'on peut manipuler en programmation. On fait ça pour extraire des données, pour manipuler du contenu web, ou pour automatiser des interactions avec des sites.

## How to - Comment faire :

```gleam
// Imaginons que vous importez une librairie de parsing HTML.
import gleam/html

pub fn extract_titles(html: String) -> List(String) {
  // Utilisez la librairie pour parser le HTML et obtenir les titres.
  ...
}

// Utilisez votre fonction.
let titles = extract_titles("<html>...</html>")
io.debug(titles) // Affiche quelque chose comme: ["Titre 1", "Titre 2", "Titre 3"]
```

(Remarque: Gleam n'a pas de librairie de parsing HTML intégrée. Vous devrez faire appel à une librairie externe ou interfacer avec du code Erlang/Elixir.)

## Deep Dive - Plongée en profondeur

History:
Le parsing HTML n'est pas nouveau. Avec l'évolution du web, ces techniques sont devenues essentielles pour le web scraping et pour les tests automatisés de sites web.

Alternatives:
Avant Gleam, Erlang et Elixir étaient souvent utilisés pour le parsing HTML, avec des librairies comme `Floki`. En fait, vous pouvez toujours utiliser ces librairies avec Gleam via les capacités d’interopérabilité de la plate-forme BEAM.

Implementation:
Faire du parsing de HTML à la main peut être compliqué car le HTML n'est pas toujours bien formé. C'est pourquoi la majorité des programmeurs Gleam préfèreront utiliser des librairies existantes, qui se chargent de gérer les cas tordus et les subtilités de la spécification HTML.

## See Also - Voir aussi

- Documentation officielle de Gleam: https://gleam.run/
- Repo GitHub pour découvrir des paquets Gleam: https://github.com/gleam-lang
- Documentation de la librairie `Floki` pour Elixir (utilisable depuis Gleam): https://hexdocs.pm/floki
- Guide pour interfacer Gleam avec Erlang/Elixir: https://gleam.run/book/tour/erlang-interop.html
