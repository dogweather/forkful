---
title:                "Mettre une chaîne de caractères en majuscules"
date:                  2024-01-19
html_title:           "C: Mettre une chaîne de caractères en majuscules"
simple_title:         "Mettre une chaîne de caractères en majuscules"

tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fr/kotlin/capitalizing-a-string.md"
---

{{< edit_this_page >}}

## What & Why?
La capitalisation d'une chaîne de caractères convertit la première lettre en majuscule. Les programmeurs l'utilisent pour normaliser les entrées des utilisateurs ou pour suivre les conventions de formatage, comme les noms propres ou les titres.

## How to:
Kotlin simplifie la capitalisation avec la fonction `replaceFirstChar`. Voici comment ça marche :

```kotlin
fun main() {
    val phrase = "voici un exemple."
    val phraseCapitalizee = phrase.replaceFirstChar { if (it.isLowerCase()) it.titlecase() else it.toString() }

    println(phraseCapitalizee) // Affiche: Voici un exemple.
}
```

Si vous avez un texte entier à transformer :

```kotlin
fun String.capitalizeEachWord(): String =
    split(" ").joinToString(" ") { it.replaceFirstChar { char -> if (char.isLowerCase()) char.titlecase() else char.toString() } }

fun main() {
    val texte = "un autre exemple de texte."
    println(texte.capitalizeEachWord()) // Affiche: Un Autre Exemple De Texte.
}
```

## Deep Dive
Historiquement, Kotlin utilisait `capitalize()` pour majusculer le premier caractère, mais cette fonction a été dépréciée à partir de Kotlin 1.5 au profit de `replaceFirstChar` pour une meilleure gestion des cas spécifiques comme les caractères unicode.

Il y a des alternatives, comme `toUpperCase()` ou `toLowerCase()` sur tous les caractères, pas juste le premier. Mais attention, ces méthodes affectent toute la chaîne et pas uniquement la première lettre.

Niveau réalisation, `replaceFirstChar` est intelligent. Il vérifie si le premier caractère a besoin d'être changé avant de procéder, ce qui en fait une solution efficace pour ne pas créer inutilement de nouvel objet `String`.

## See Also
- Kotlin Documentation: [Text](https://kotlinlang.org/docs/basic-types.html#string-literals)
- Article sur la dépréciation de `capitalize`: [Restrictive String Capitalization](https://blog.jetbrains.com/kotlin/2021/05/kotlin-1-5-0-released/#restrictive_string_capitalization)
- Guide du style Kotlin: [Coding Conventions](https://kotlinlang.org/docs/coding-conventions.html#functions-vs-properties)
