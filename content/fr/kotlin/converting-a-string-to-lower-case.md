---
title:                "Kotlin: Convertir une chaîne de caractères en minuscules"
simple_title:         "Convertir une chaîne de caractères en minuscules"
programming_language: "Kotlin"
category:             "Kotlin"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fr/kotlin/converting-a-string-to-lower-case.md"
---

{{< edit_this_page >}}

## Pourquoi

La conversion d'une chaîne de caractères en minuscules est une tâche courante en programmation. Elle permet de normaliser les données et de faciliter leur traitement ultérieur. Dans cet article, nous allons voir comment effectuer cette opération en utilisant Kotlin.

## Comment faire

La conversion en minuscules peut être faite de plusieurs manières en Kotlin. Voici quelques exemples de code avec leur sortie respective :

```Kotlin
// Exemple 1
var str = "KOTLIN"
println(str.toLowerCase())

// sortie : kotlin

// Exemple 2
val input = "jE sUiS uNe cHaîNe De CarAcTèrEs"
val output = input.toLowerCase()
println(output)

// sortie : je suis une chaîne de caractères
```

## Plongée en profondeur

Il existe deux façons principales de convertir une chaîne de caractères en minuscules en Kotlin : en utilisant la fonction prédéfinie `toLowerCase()` ou en utilisant l'extension `toLowerCase()` de la classe `CharSequence`.

La méthode `toLowerCase()` peut être utilisée directement sur une chaîne de caractères et renvoie la chaîne en minuscules. Cette méthode est plus concise et intuitive.

L'extension `toLowerCase()` de la classe `CharSequence` est plus flexible car elle peut être appliquée à différents types de données, tels que des chaînes de caractères, des tableaux de caractères ou des objets implémentant l'interface `CharSequence`. Cependant, elle est un peu plus verbeuse en terme de syntaxe.

Il est également important de noter que la conversion en minuscules dépend de la locale par défaut de l'application. Si vous souhaitez convertir la chaîne en minuscules conformément à une locale spécifique, vous pouvez utiliser la méthode `toLowerCase(locale: Locale)` ou l'extension `toLowerCase(locale: Locale)` avec la locale souhaitée.

## Voir aussi

- [Documentation officielle de Kotlin sur les chaînes de caractères](https://kotlinlang.org/docs/reference/basic-types.html#strings)
- [Tutoriel sur les string templates en Kotlin](https://blog.mindorks.com/string-templates-in-kotlin)
- [Article sur l'utilisation des expressions régulières en Kotlin](https://www.bignerdranch.com/blog/kotlin-regular-expressions-and-the-future/)