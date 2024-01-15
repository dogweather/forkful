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

## Pourquoi

Remplacer du texte peut être une tâche fastidieuse et chronophage, surtout si vous devez le faire manuellement. Heureusement, Elm offre une solution simple et efficace pour automatiser cette tâche et gagner du temps.

## Comment faire

```Elm
import String

main =
    let
        string = "Bonjour le monde!"
        newString = String.replace "Bonjour" "Hello" string
    in
        "String de départ : " ++ string ++ "\nNouvelle string : " ++ newString
```

Output : String de départ : Bonjour le monde! 

Nouvelle string : Hello le monde!

En utilisant la fonction `String.replace`, vous pouvez facilement remplacer un mot ou une phrase dans une chaîne de caractères. La fonction prend trois paramètres : la chaîne de caractères initiale, le mot/la phrase à remplacer et le nouveau mot/phrase. Vous pouvez également utiliser les bibliothèques `Regex` et `List` pour des cas plus complexes.

## Plongée en profondeur

La fonction `String.replace` utilise des expressions régulières en interne pour trouver et remplacer le texte. Cela signifie que vous pouvez utiliser des symboles spéciaux pour des remplacements plus précis. Par exemple, vous pouvez utiliser `Regex.fromStrings`pour remplacer toutes les occurrences d'un groupe de lettres dans une chaîne de caractères.

## Voir aussi

- [Documentation officielle Elm](https://elm-lang.org/docs)
- [Tutoriel sur les expressions régulières en Elm](https://www.effectivereasoning.be/blog/2017/01/27/upgrading-elm-regular-expressions/)
- [Exemples de cas d'utilisation pour la fonction String.replace](https://github.com/rtfeldman/elm-string-replace/blob/master/examples/Examples.elm)