---
title:                "Mettre en majuscule une chaîne"
aliases:
- /fr/elm/capitalizing-a-string/
date:                  2024-02-03T19:04:55.603704-07:00
model:                 gpt-4-0125-preview
simple_title:         "Mettre en majuscule une chaîne"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fr/elm/capitalizing-a-string.md"
changelog:
  - 2024-02-03, gpt-4-0125-preview, translated from English
---

{{< edit_this_page >}}

## Quoi & Pourquoi ?

Capitaliser une chaîne implique de transformer le caractère initial d'une chaîne donnée en majuscule tout en gardant le reste en minuscule, souvent pour des raisons de formatage standardisé ou de lisibilité. Les programmeurs effectuent fréquemment cette tâche pour s'assurer que les données sont présentées de manière cohérente, en particulier dans les interfaces utilisateur ou lors du traitement et de l'affichage des entrées utilisateur.

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
