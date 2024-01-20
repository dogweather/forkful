---
title:                "Mettre en majuscule une chaîne de caractères"
html_title:           "Elm: Mettre en majuscule une chaîne de caractères"
simple_title:         "Mettre en majuscule une chaîne de caractères"
programming_language: "Elm"
category:             "Elm"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fr/elm/capitalizing-a-string.md"
---

{{< edit_this_page >}}

## Quoi & Pourquoi?

Capitaliser une chaîne signifie transformer la première lettre de chaque mot en majuscule. Les programmeurs font cela pour améliorer la présentation du texte et rendre les titres ou les noms de variables plus visibles.

## Comment faire:

En Elm, vous pouvez capitaliser une chaîne à l'aide de la fonction `toUpper` appliquée à la première lettre et en concaténant le reste de la chaîne. Voici un exemple de mise en œuvre:

``` Elm
import String

capitalize : String -> String
capitalize str =
    case String.uncons str of
        Nothing ->
            ""

        Just ( first, rest ) ->
            String.toUpper (String.fromChar first) ++ rest
```
Ainsi, si vous exécutez:

``` Elm
capitalize "bonjour, monde"
```

La sortie sera:

``` Elm
"Bonjour, Monde"
```

## Vue approfondie:

La fonction `capitalize` n'est pas intégrée dans le langage Elm contradictoirement à d'autres langages de programmation tels que Java ou Python. Cette fonctionnalité est plus une convention dans le monde de la programmation pour améliorer l'apparence du texte pour l'utilisateur.

Pour capitaliser une chaîne en Elm, la meilleure façon est de convertir la première lettre en majuscule à l'aide de la fonction 'toUpper' tout en laissant le reste de la chaîne inchangé. Cette approche est simple et efficace, mais elle n'est pas adaptée pour les chaînes multi-mots. Pour capitaliser chaque mot dans une chaîne, une approche plus complexe serait nécessaire, comme séparer la chaîne en mots, capitaliser chaque mot puis les réunir.

## Voir Aussi:

Pour plus de détails, consultez les ressources en ligne suivantes:

- Documentation Elm sur les chaînes de caractères: https://elm-lang.org/docs/string
- Discussion sur le forum Elm concernant la capitalisation des chaînes: https://discourse.elm-lang.org/t/capitalization-in-string/338
- Guide de style Elm officiel: https://github.com/elm/style-guide/blob/master/README.md