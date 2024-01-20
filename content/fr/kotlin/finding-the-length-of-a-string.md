---
title:                "Trouver la longueur d'une chaîne"
html_title:           "Go: Trouver la longueur d'une chaîne"
simple_title:         "Trouver la longueur d'une chaîne"
programming_language: "Kotlin"
category:             "Kotlin"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fr/kotlin/finding-the-length-of-a-string.md"
---

{{< edit_this_page >}}

## Qu'est-ce et Pourquoi?

Trouver la longueur d'une chaîne consiste à déterminer combien de caractères elle contient. Les programmeurs le font pour effectuer des opérations comme la validation de données, la manipulation de chaînes et la résolution de problèmes liés à l'algorithme.

## Comment faire:

Voici comment vous pouvez le faire en Kotlin. Utilisez `length` sur un objet `String`.

```Kotlin
val str = "Bonjour le monde!"
println("La longueur de la chaîne est: ${str.length}")
```

Le code ci-dessus affiche:

```Kotlin
La longueur de la chaîne est: 18
```

## Analyse approfondie

Kotlin, en tant que langage de programmation moderne, offre l'attribut `.length` pour simplifier le calcul de la longueur de la chaîne. Dans le passé, dans des langages plus anciens comme C, vous deviez utiliser une fonction de bibliothèque telle que `strlen`.

Il existe des alternatives à `.length` en Kotlin. Par exemple, vous pouvez utiliser l'expression `fold` pour compter les caractères:

```Kotlin
val str = "Bonjour le monde!"
val longueur = str.fold(0) { compteur, _ -> compteur + 1 }
println("La longueur de la chaîne est: $longueur")
```

Cela dit, l'utilisation de `.length` est plus conventionnelle et recommandée pour des raisons de lisibilité et de performances.

Concernant les implémentations détaillées, `length` en Kotlin est une propriété de `String` et non une fonction, ce qui la rend très agréable à utiliser. D'un point de vue interne, `length` est une fonction JNI (Java Native Interface) qui renvoie la longueur du tableau de caractères sous-jacent de la chaîne.

## Voir aussi

1. Kotlin String.length - https://kotlinlang.org/api/latest/jvm/stdlib/kotlin/-string/length.html
2. Kotlin fold - https://kotlinlang.org/api/latest/jvm/stdlib/kotlin.collections/fold.html
3. Documentation officielle sur String in Kotlin - https://kotlinlang.org/api/latest/jvm/stdlib/kotlin/-string/ 

N'hésitez pas à consulter ces liens pour une compréhension plus approfondie et des explorations supplémentaires.