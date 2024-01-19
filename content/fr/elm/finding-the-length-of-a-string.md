---
title:                "Trouver la longueur d'une chaîne"
html_title:           "Go: Trouver la longueur d'une chaîne"
simple_title:         "Trouver la longueur d'une chaîne"
programming_language: "Elm"
category:             "Elm"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fr/elm/finding-the-length-of-a-string.md"
---

{{< edit_this_page >}}

## Qu'est-ce et Pourquoi?

La découverte de la longueur d'une chaîne consiste à déterminer le nombre de caractères qu'elle contient. Les programmeurs le font pour diverses raisons, comme valider les entrées de l'utilisateur ou manipuler les données textuelles.

## Comment faire :
```Elm
import String

main =
  let str = "Bonjour le monde!"
  in String.length str
```
Dans ce code, "`Bonjour le monde!`" a une longueur de 17.

## Plongée en profondeur:

Historiquement, la longueur des chaînes était coûteuse en termes de temps de calcul, mais Elm utilise une implémentation efficace. Il y a des alternatives dans Elm pour travailler avec les chaînes sans connaître leur longueur, par exemple en utilisant des flux infinis et des listes paresseuses. Cependant, connaître la longueur d'une chaîne peut souvent rendre le code plus rapide et plus simple.

L'opération `String.length` mesure le nombre de points de code Unicode dans la chaîne, pas le nombre d'octets qui la stockent ou le nombre de "caractères" dans un certain sens linguistique ou typographique. Le temps d'exécution est linéaire par rapport à la longueur de la chaîne.

## À voir aussi:

Consultez les ressources suivantes pour plus d'informations:

- Documentation Elm sur les chaînes: https://package.elm-lang.org/packages/elm/core/latest/String#length
- Guide Elm sur les chaînes : https://guide.elm-lang.org/appendix/types_as_sets.html
- Documentation de Mozilla sur les chaînes: https://developer.mozilla.org/fr/docs/Web/JavaScript/Reference/Objets_globaux/String/length