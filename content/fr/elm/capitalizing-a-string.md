---
title:    "Elm: Mettre en majuscule une chaîne de caractères"
keywords: ["Elm"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/fr/elm/capitalizing-a-string.md"
---

{{< edit_this_page >}}

## Pourquoi

La capitalisation des chaînes de caractères est une tâche courante en programmation, surtout lorsqu'il s'agit de manipuler du texte. Heureusement, en utilisant Elm, cette tâche peut être facilement accomplie grâce à une fonction intégrée.

## Comment procéder

Pour capitaliser une chaîne de caractères en Elm, il suffit d'utiliser la fonction `String.toUpper` et de lui donner la chaîne de caractères que vous souhaitez capitaliser en tant qu'argument. Voici un exemple de code pour une chaîne de caractères en minuscules :

```Elm
string = "bonjour"
String.toUpper string
```

Lorsque vous exécutez ce code, la sortie sera :

```Elm
"BONJOUR"
```

Si vous souhaitez capitaliser seulement la première lettre d'une chaîne de caractères, vous pouvez utiliser la fonction `String.capitalize` au lieu de `String.toUpper`.

## Plongeon en profondeur

Il est important de noter que la fonction `String.toUpper` et la fonction `String.capitalize` ne modifient pas directement la chaîne de caractères d'origine, mais renvoient plutôt une nouvelle chaîne de caractères avec les modifications souhaitées. Cela permet de conserver l'intégrité de la chaîne d'origine et d'éviter des erreurs inattendues dans votre code.

De plus, ces fonctions ne fonctionnent que pour les caractères alphabétiques. Les chiffres, les symboles ou les espaces blancs ne seront pas affectés par ces fonctions.

## Voir aussi
- Documentation sur la fonction `String.toUpper` : https://package.elm-lang.org/packages/elm/core/latest/String#toUpper
- Documentation sur la fonction `String.capitalize` : https://package.elm-lang.org/packages/elm/core/latest/String#capitalize