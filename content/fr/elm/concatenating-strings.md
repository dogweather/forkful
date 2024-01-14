---
title:                "Elm: Concaténation de chaînes de caractères"
programming_language: "Elm"
category:             "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fr/elm/concatenating-strings.md"
---

{{< edit_this_page >}}

## Pourquoi

Concaténer des chaînes de caractères (ou strings en anglais) est une tâche très commune en programmation, surtout lorsqu'on travaille avec des données. Cela permet de combiner différentes chaînes pour créer une seule chaîne plus complexe et complète.

## Comment faire

En Elm, la concaténation de chaînes se fait en utilisant l'opérateur "+" entre deux chaînes, comme dans l'exemple suivant :

```Elm
nom = "Jean"
prenom = "Dupont"
nomComplet = nom + " " + prenom

```

Dans cet exemple, la variable "nomComplet" contiendra la chaîne "Jean Dupont". On peut aussi concaténer des chaînes avec des valeurs numériques, en les convertissant en chaîne grâce à la fonction "String.fromInt" pour les entiers et "String.fromFloat" pour les décimaux.

## Plongée en profondeur

Il est important de noter que la concaténation de chaînes peut être un processus gourmand en ressources, surtout lorsque l'on travaille avec de grandes quantités de données. Cela peut ralentir l'exécution du code et causer des problèmes de performances. Pour cette raison, il est conseillé d'utiliser la fonction "++" pour concaténer plus de deux chaînes en une seule fois. Par exemple :

```Elm
baseUrl = "https://monsite.com/"
endpoint = "/users"
params = "?page=1"
url = baseUrl ++ endpoint ++ params
```

Cette méthode est plus efficace car elle combine toutes les chaînes en une seule, plutôt que d'exécuter plusieurs opérations de concaténation à la suite.

## Voir aussi

- [Documentation officielle Elm sur la concaténation de chaînes](https://elm-lang.org/docs/strings#concatenation)
- [Exemples de manipulation de chaînes en Elm](https://package.elm-lang.org/packages/mpizenberg/elm-string-utils/latest/String.Utils)
- [Tutoriel sur la manipulation de données en Elm](https://www.elm-tutorial.org/fr/03-saisie-utilisateur/01-listes-et-vue.html)