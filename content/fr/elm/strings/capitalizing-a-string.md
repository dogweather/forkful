---
changelog:
- 2024-02-03, gpt-4-0125-preview, translated from English
date: 2024-02-03 19:04:55.603704-07:00
description: "Comment faire : Dans Elm, il n'y a pas de fonction int\xE9gr\xE9e sp\xE9\
  cifiquement pour capitaliser les cha\xEEnes. Cependant, vous pouvez facilement y\
  \ parvenir en\u2026"
lastmod: '2024-03-13T22:44:57.670906-06:00'
model: gpt-4-0125-preview
summary: "Dans Elm, il n'y a pas de fonction int\xE9gr\xE9e sp\xE9cifiquement pour\
  \ capitaliser les cha\xEEnes."
title: "Mettre en majuscule une cha\xEEne"
weight: 2
---

## Comment faire :
Dans Elm, il n'y a pas de fonction intégrée spécifiquement pour capitaliser les chaînes. Cependant, vous pouvez facilement y parvenir en utilisant les fonctions du module `String` intégré comme `toUpper`, `toLower`, `left`, et `dropLeft`.

```elm
capitalize : String -> String
capitalize str =
    if String.isEmpty str then
        ""
    else
        String.toUpper (String.left 1 str) ++ String.toLower (String.dropLeft 1 str)

-- Exemple d'utilisation
main =
    String.toList "hello world" |> List.map capitalize |> String.join " "
    -- Résultat : "Hello World"
```

Pour des scénarios plus complexes ou si vous préférez utiliser une bibliothèque qui offre une manière directe de capitaliser les chaînes, vous pourriez envisager un package tiers tel que `elm-community/string-extra`. Cependant, à ma dernière mise à jour, l'écosystème Elm encourage à traiter de telles tâches en utilisant des fonctions intégrées pour garder le langage et les projets épurés.

```elm
import String.Extra as StringExtra

-- Dans le cas où il y a une fonction `capitalize` dans une bibliothèque tierce
capitalizeWithLibrary : String -> String
capitalizeWithLibrary str =
    StringExtra.capitalize str

-- Exemple d'utilisation avec une fonction hypothétique de bibliothèque
main =
    "this is elm" |> capitalizeWithLibrary
    -- Résultat hypothétique : "This is elm"
```

Vérifiez toujours le dépôt de packages Elm pour les bibliothèques les plus récentes et les plus préférées pour la manipulation de chaînes si vous recherchez des fonctionnalités supplémentaires au-delà de la bibliothèque standard.
