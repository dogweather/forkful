---
title:                "Elm: Majuscule d'une chaîne de caractères"
programming_language: "Elm"
category:             "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fr/elm/capitalizing-a-string.md"
---

{{< edit_this_page >}}

# Pourquoi

Le langage de programmation Elm est de plus en plus populaire parmi les développeurs pour ses performances et sa stabilité. L'une des fonctionnalités clés qu'offre Elm est la manipulation des chaînes de caractères, et notamment la capitalisation d'une chaîne. Dans cet article, nous allons voir pourquoi il peut être utile de capitaliser une chaîne et comment le faire en Elm.

## Comment faire

Pour capitaliser une chaîne de caractères en Elm, nous allons utiliser la fonction `String.capitalize`. Elle prend en paramètre une chaîne de caractères et renvoie cette même chaîne avec la première lettre en majuscule. Voici un exemple de code :

```elm
String.capitalize "bonjour" -- renvoie "Bonjour"
```

Nous pouvons également utiliser `String.toUpper` pour mettre l'ensemble de la chaîne en majuscules :

```elm
String.toUpper "bonjour" -- renvoie "BONJOUR"
```

Nous pouvons également utiliser `String.toLower` pour mettre l'ensemble de la chaîne en minuscules :

```elm
String.toLower "BONJOUR" -- renvoie "bonjour"
```

Il est également possible de capitaliser uniquement la première lettre d'une chaîne sans changer le reste de la casse avec la fonction `String.toHeadUpper` :

```elm
String.toHeadUpper "bonjour" -- renvoie "Bonjour"
String.toHeadUpper "BONJOUR" -- renvoie "BONJOUR"
```

Vous pouvez également utiliser ces fonctions avec des variables pour rendre votre code plus dynamique. Par exemple :

```elm
let
  chaine = "bonjour"
in
  String.capitalize chaine -- renvoie "Bonjour"
```

## Plongée en profondeur

Maintenant que nous avons vu comment capitaliser une chaîne de caractères en Elm, parlons un peu plus en détail de pourquoi cela peut être utile. Parfois, nous voulons afficher une chaîne dans un format spécifique, par exemple pour la titrer ou pour respecter une convention de nommage. Dans ce cas, la fonction `String.capitalize` peut nous être utile.

De plus, si vous manipulez des données dans votre application Elm, il se peut que vous ayez besoin de comparer des chaînes de caractères. En capitalisant toutes les chaînes, vous vous assurez qu'elles ont toutes la même casse, ce qui facilite grandement les comparaisons.

Enfin, utiliser la fonction `String.capitalize` peut également rendre votre code plus lisible pour vous et pour les autres développeurs, en mettant en évidence les parties importantes de vos chaînes.

# Voir aussi

- Documentation officielle de Elm sur la manipulation des chaînes de caractères : https://guide.elm-lang.org/strings/
- Tutoriel sur la mise en forme des chaînes de caractères en Elm : https://www.tutorialspoint.com/elm/elm_strings.htm
- Exemple pratique de l'utilisation de `String.capitalize` : https://www.youtube.com/watch?v=oCznp67hOh0