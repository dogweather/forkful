---
title:                "Kotlin: Majuscule d'une chaîne de caractères"
programming_language: "Kotlin"
category:             "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fr/kotlin/capitalizing-a-string.md"
---

{{< edit_this_page >}}

## Pourquoi

L'une des tâches les plus courantes en programmation est la manipulation de chaînes de caractères. Et parfois, nous avons besoin de capitaliser une chaîne de caractères pour des raisons de mise en forme ou de cohérence avec d'autres éléments de notre code. Dans cet article, nous allons discuter de manière détaillée comment capitaliser une chaîne de caractères en utilisant Kotlin.

## Comment faire

La méthode la plus simple pour capitaliser une chaîne de caractères en utilisant Kotlin est d'utiliser la fonction `capitalize()` incluse dans la librairie standard. Voyons un exemple concret:

```Kotlin
var name = "john doe"
println(name.capitalize())
```

Output: "John doe"

Nous pouvons également capitaliser uniquement la première lettre d'une chaîne de caractères en utilisant la fonction `replaceFirstChar()` et en manipulant la première lettre pour la convertir en majuscule:

```Kotlin
var message = "hello world"
message = message.replaceFirstChar { it.uppercase() }
println(message)
```
Output: "Hello world"

## Plongeons plus en profondeur

La méthode `capitalize()` est en fait un raccourci pour transformer la première lettre en majuscule et mettre toutes les autres lettres en minuscule. Si nous voulons uniquement mettre la première lettre en majuscule sans modifier les autres lettres, nous pouvons utiliser `replaceFirstChar()` comme mentionné précédemment. Mais nous pouvons également utiliser `toUpperCase()` sur la première lettre et `toLowerCase()` sur le reste de la chaîne de caractères pour obtenir le même résultat.

En outre, la méthode `capitalize()` ne prend en compte que la première lettre du premier mot d'une chaîne de caractères. Si nous voulons capitaliser chaque mot dans une chaîne de caractères, nous pouvons diviser la chaîne en utilisant `split()` et réappliquer `capitalize()` sur chaque élément du tableau retourné par `split()`.

Finalement, il est important de noter que ces méthodes ne changent pas la chaîne de caractères initiale. Elles retournent une nouvelle chaîne de caractères avec les modifications faites. Si vous souhaitez modifier la chaîne de caractères d'origine, il faudra l'assigner à la variable initiale comme nous l'avons fait dans les exemples ci-dessus.

## Voir aussi

- [La documentation officielle de la librairie standard de Kotlin](https://kotlinlang.org/api/latest/jvm/stdlib/kotlin.collections/capitalize.html)
- [La documentation officielle de la fonction `replaceFirstChar()`](https://kotlinlang.org/api/latest/jvm/stdlib/kotlin.text/replace-first-char.html)
- [Un guide complet sur les chaînes de caractères en Kotlin](https://blog.kotlin-academy.com/strings-and-chars-in-kotlin-7ac2c2d512f8)