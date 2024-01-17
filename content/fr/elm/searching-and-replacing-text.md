---
title:                "Rechercher et remplacer du texte"
html_title:           "Elm: Rechercher et remplacer du texte"
simple_title:         "Rechercher et remplacer du texte"
programming_language: "Elm"
category:             "Elm"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fr/elm/searching-and-replacing-text.md"
---

{{< edit_this_page >}}

Qu'est-ce que le remplacement de texte et pourquoi les programmeurs le font-ils ?

Le remplacement de texte est une technique qui consiste à trouver un morceau de texte dans un fichier ou un document et à le remplacer par un autre. Les programmeurs le font pour faciliter la modification de leur code, corriger des erreurs et améliorer l'efficacité de leur travail.

Comment faire :

Elm offre une fonction intégrée pour effectuer cette tâche, appelée replace. Voici un exemple de code montrant son utilisation :

```Elm
replace "Bonjour" "Hello" "Bonjour tout le monde" -- sortie: "Hello tout le monde"
```

Nous avons utilisé la fonction replace en spécifiant la chaîne de texte à remplacer (Bonjour) et par quoi la remplacer (Hello), ainsi que le texte dans lequel effectuer le remplacement (Bonjour tout le monde). Le résultat renvoyé est le texte modifié, avec le remplacement effectué.

Pour effectuer un remplacement sur plusieurs occurrences d'un texte, il suffit d'utiliser la fonction replaceMultiple, voici un exemple :

```Elm
replaceMultiple "el" "le" "element électronique" -- sortie: "lement électronique"
```

Plongée en profondeur :

Le remplacement de texte est une pratique courante dans de nombreux langages de programmation, comme Javascript ou Python. D'autres alternatives incluent l'utilisation d'expressions régulières ou la création de scripts personnalisés pour effectuer le remplacement.

Elm offre également la possibilité d'utiliser des expressions régulières pour effectuer des remplacements avec la fonction replaceRegex. Cette fonction utilise le moteur de recherche Google RE2 pour améliorer les performances.

Voir aussi :

- Documentation officielle d'Elm sur le remplacement de texte : https://package.elm-lang.org/packages/elm/core/latest/String#replace
- Tutoriel sur les expressions régulières en Elm : https://elmprogramming.com/regular-expressions-in-Elm.html