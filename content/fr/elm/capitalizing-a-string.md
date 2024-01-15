---
title:                "Majusculation d'une chaîne de caractères"
html_title:           "Elm: Majusculation d'une chaîne de caractères"
simple_title:         "Majusculation d'une chaîne de caractères"
programming_language: "Elm"
category:             "Elm"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fr/elm/capitalizing-a-string.md"
---

{{< edit_this_page >}}

## Pourquoi

Tout d'abord, pourquoi irions-nous vouloir capitaliser une chaîne de caractères en Elm ? La réponse est simple : pour une meilleure lisibilité et pour respecter certaines conventions de codage.

## Comment faire

Pour capitaliser une chaîne de caractères en Elm, il existe une fonction prédéfinie appelée `String.toUpper` qui prend une chaîne de caractères en entrée et renvoie une version en majuscules de cette chaîne. Voici un exemple de code avec une chaîne de caractères définie et l'utilisation de la fonction `String.toUpper` :

```elm
myString = "bonjour"
capitalizedString = String.toUpper myString
```

La variable `capitalizedString` contiendra alors la valeur "BONJOUR". On peut également directement utiliser la fonction `String.toUpper` dans une expression, comme ceci :

```elm
message = "bienvenue " ++ String.toUpper "utilisateur"
```

Dans cet exemple, la variable `message` contiendra la valeur "bienvenue UTILISATEUR". On peut également utiliser des variables à l'intérieur de la fonction `String.toUpper` si on le souhaite.

## Plongeon en profondeur

En plus de la fonction `String.toUpper`, il existe d'autres options pour capitaliser une chaîne de caractères en Elm, telles que l'utilisation de la bibliothèque `elm-community/string-extra` ou la création de sa propre fonction personnalisée en utilisant des fonctions de manipulation de chaînes comme `String.toList` et `String.fromList`.

Il est également important de noter que la fonction `String.toUpper` ne fonctionne pas sur tous les caractères, en particulier ceux avec des accents ou des caractères spéciaux. Dans ce cas, il est nécessaire d'utiliser des fonctions de conversion spécifiques, comme `String.toUpperWithLocale` pour prendre en compte les spécificités de chaque langue.

## Voir aussi

Pour en savoir plus sur les fonctions de manipulation de chaînes en Elm, n'hésitez pas à consulter la documentation officielle sur les chaînes de caractères ainsi que la bibliothèque `elm/community/string-extra`.

- [Documentation officielle - Chaînes de caractères](https://package.elm-lang.org/packages/elm/core/latest/String)
- [Bibliothèque elm/community/string-extra](https://package.elm-lang.org/packages/elm-community/string-extra/latest)