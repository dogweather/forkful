---
title:                "Concaténation de chaînes de caractères"
html_title:           "Elm: Concaténation de chaînes de caractères"
simple_title:         "Concaténation de chaînes de caractères"
programming_language: "Elm"
category:             "Elm"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fr/elm/concatenating-strings.md"
---

{{< edit_this_page >}}

## Pourquoi
La concaténation de chaînes de caractères est une opération courante en programmation, utilisée pour combiner plusieurs chaînes en une seule. Elle est particulièrement utile pour créer des messages dynamiques, des requêtes d'API ou simplement pour améliorer la lisibilité du code.

## Comment faire
La syntaxe pour concaténer des chaînes en Elm est assez simple : il suffit d'utiliser l'opérateur `++` entre deux chaînes pour les combiner. Voyons un exemple concret :

```Elm
message = "Bonjour " ++ "mon ami"
```

Dans cet exemple, la variable `message` contiendra la chaîne "Bonjour mon ami". Nous pouvons également concaténer plusieurs chaînes à la fois :

```Elm
greeting = "Salut " ++ "à tous " ++ "les amis"
```

La variable `greeting` contiendra "Salut à tous les amis".

## Plongée en profondeur
En Elm, les chaînes de caractères sont en fait des listes de caractères. Cela signifie qu'il est possible de concaténer des listes de caractères avec l'opérateur `++`. Par exemple :

```Elm
list = ['h', 'e', 'l', 'l', 'o'] ++ [' ', 'w', 'o', 'r', 'l', 'd']
```

Dans cet exemple, la variable `list` contiendra la liste de caractères ['h', 'e', 'l', 'l', 'o', ' ', 'w', 'o', 'r', 'l', 'd']. Cette liste peut ensuite être convertie en chaîne de caractères à l'aide de la fonction `String.fromList`.

De plus, l'utilisation de l'opérateur `++` peut également être combinée avec des variables, des expressions et même des fonctions pour créer des chaînes de caractères dynamiques et personnalisées.

## Voir aussi
- Documentation officielle Elm sur la concaténation de chaînes : https://guide.elm-lang.org/strings/concatenation.html
- Tutoriel sur la manipulation des chaînes en Elm : https://marmelab.com/blog/2019/06/25/how-to-manipulate-strings-in-elm.html
- Exemples pratiques d'utilisation de la concaténation de chaînes en Elm : https://dev.to/nhcodran/elm-1-fundamentals-of-programming-4jk3