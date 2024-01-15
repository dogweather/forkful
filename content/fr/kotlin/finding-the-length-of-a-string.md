---
title:                "Trouver la longueur d'une chaîne de caractères"
html_title:           "Kotlin: Trouver la longueur d'une chaîne de caractères"
simple_title:         "Trouver la longueur d'une chaîne de caractères"
programming_language: "Kotlin"
category:             "Kotlin"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fr/kotlin/finding-the-length-of-a-string.md"
---

{{< edit_this_page >}}

## Pourquoi

Il peut être utile de connaître la longueur d'une chaîne de caractères dans un programme Kotlin, par exemple pour valider une entrée utilisateur ou pour manipuler des données. Dans cet article, nous allons explorer comment trouver la longueur d'une chaîne en utilisant Kotlin.

## Comment faire

Pour trouver la longueur d'une chaîne en Kotlin, nous pouvons utiliser la fonction `length()` comme ceci:

```
Kotlin val str = "Bonjour"
println(str.length()) // Output: 7
```

Nous pouvons également utiliser la propriété `length` directement sur une chaîne comme ceci:

```
Kotlin val str = "Bonjour"
println(str.length) // Output: 7
```

Si nous voulons trouver la longueur d'une chaîne vide, la fonction `length()` nous donnera un résultat de 0.

```
Kotlin val str = ""
println(str.length()) // Output: 0
```

## Plongez plus en profondeur

En Kotlin, les chaînes de caractères sont des objets de type `String` et la fonction `length()` est une méthode de cet objet. Cette méthode compte le nombre de caractères dans une chaîne, en incluant les espaces et les caractères spéciaux.

Nous pouvons également utiliser la fonction `size()` sur une chaîne en Kotlin pour trouver sa longueur. Cependant, la différence entre `length()` et `size()` est que `length()` est une méthode spécifique du langage, tandis que `size()` est une méthode d'extension qui peut être utilisée sur d'autres types de collections notamment.

Nous pouvons également utiliser des chaînes de caractères unicode en Kotlin. La fonction `length()` retournera le nombre de caractères unicode dans la chaîne, tandis que la propriété `length` retournera le nombre de separateurs unicode.

## Voir aussi

- [Documentation sur les chaînes de caractères en Kotlin](https://kotlinlang.org/docs/reference/basic-types.html#strings)
- [Tutoriel interactif sur les chaînes de caractères en Kotlin](https://play.kotlinlang.org/koans/overview)