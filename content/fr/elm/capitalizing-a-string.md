---
title:                "Mettre une chaîne de caractères en majuscules"
date:                  2024-01-19
html_title:           "C: Mettre une chaîne de caractères en majuscules"
simple_title:         "Mettre une chaîne de caractères en majuscules"
programming_language: "Elm"
category:             "Elm"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fr/elm/capitalizing-a-string.md"
---

{{< edit_this_page >}}

## What & Why?
Capitaliser une chaîne signifie transformer toutes ses lettres en majuscules. Les développeurs utilisent cette technique pour standardiser le texte pour des comparaisons ou pour l'afficher de manière plus visible et formelle.

## How to:
Elm n'a pas de fonction intégrée pour capitaliser une chaîne entière, on doit donc créer la sienne ou utiliser un package externe.

```elm
import String

capitalize : String -> String
capitalize str =
  String.toUpper str

-- Utilisation:
resultat = capitalize "bonjour, elm!"

-- Sortie:
-- "BONJOUR, ELM!"
```

## Deep Dive
Elm, axé sur la simplicité, offre `String.toUpper` pour transformer des lettres en majuscules. Historiquement, différentes langues traitent la capitalisation différemment; Elm fait simple avec `toUpper` et `toLower` pour tout convertir. Si vous avez besoin de comportements plus complexes (comme capitaliser seulement la première lettre), vous devrez éventuellement chercher un package tiers comme `elm-community/string-extra`. Côté implémentation, la gestion de la casse dépend du navigateur, Elm délégant cette tâche au JavaScript sous-jacent.

## See Also
- Elm String Documentation: https://package.elm-lang.org/packages/elm/core/latest/String#toUpper
- String-Extra Package for more string operations: https://package.elm-lang.org/packages/elm-community/string-extra/latest/
