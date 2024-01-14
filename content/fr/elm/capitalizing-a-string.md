---
title:                "Elm: Majuscule d'une chaîne de caractères"
simple_title:         "Majuscule d'une chaîne de caractères"
programming_language: "Elm"
category:             "Elm"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fr/elm/capitalizing-a-string.md"
---

{{< edit_this_page >}}

## Pourquoi

Si vous êtes un programmeur Elm, vous savez probablement qu'il n'y a rien de plus frustrant que de devoir corriger des erreurs de casse dans une chaîne de caractères. Heureusement, avec la fonction `String.toUpper`, cela devient un jeu d'enfant ! Découvrez dans cet article comment capitaliser facilement une chaîne de caractères en Elm.

## Comment Faire

Pour capitaliser une chaîne de caractères en Elm, il vous suffit d'utiliser la fonction `String.toUpper` comme ceci :

```Elm
myString = "bonjour"
String.toUpper myString
```

Et voilà, vous obtenez maintenant `"BONJOUR"` comme résultat ! Vous pouvez également utiliser cette fonction directement dans une chaîne de caractères comme ceci :

```Elm
greeting = "Bonjour, je suis Eliot."
"Ce n'est qu'un petit " ++ String.toUpper greeting
```

Maintenant, le résultat sera `"Ce n'est qu'un petit BONJOUR, JE SUIS ELIOT."` ! N'est-ce pas simple ?

## Plongée En Profondeur

Maintenant que vous savez comment capitaliser une chaîne de caractères en Elm, vous pouvez également utiliser la fonction `String.toTitle` pour obtenir une chaîne de caractères avec la première lettre de chaque mot en majuscule. Par exemple :

```Elm
myString = "bonjour, je suis Eliot."
String.toTitle myString
```

Le résultat sera `"Bonjour, Je Suis Eliot."`. Vous pouvez également utiliser la fonction `String.toLower` pour obtenir une chaîne de caractères en minuscules.

## Voir Aussi

- Documentation officielle de Elm sur les chaînes de caractères : https://guide.elm-lang.org/strings/
- Tutoriel en français sur Elm : https://elm-tutorial.org/fr/
- Forum de la communauté Elm : https://discourse.elm-lang.org/