---
title:                "Kotlin: Transformation d'une chaîne de caractères en minuscules"
programming_language: "Kotlin"
category:             "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fr/kotlin/converting-a-string-to-lower-case.md"
---

{{< edit_this_page >}}

# Pourquoi Convertir une Chaîne en Caractères Minuscules en Kotlin ?

La conversion d'une chaîne de caractères en minuscules peut être une étape importante dans de nombreux projets de programmation en Kotlin. Elle peut vous aider à nettoyer et normaliser les données, faciliter les comparaisons de chaînes et améliorer la convivialité de l'interface utilisateur. Dans cet article, nous allons plonger dans les détails de la conversion des chaînes en minuscules en utilisant Kotlin.

## Comment Procéder
Pour convertir une chaîne en caractères minuscules en Kotlin, vous pouvez utiliser la fonction de la bibliothèque standard `toLowerCase()` ou la méthode `toLowerCase(Locale)` de l'objet `String`. Voici un exemple de code montrant les deux façons de convertir une chaîne en minuscules :

```Kotlin
fun main() {
    val string = "HELLO WORLD"
    
    // Utilisation de la fonction toLowerCase() de la bibliothèque standard
    val lowerCase1 = string.toLowerCase() 
    println(lowerCase1) // sortie: hello world
    
    // Utilisation de la méthode toLowerCase(Locale) de l'objet String
    val lowerCase2 = string.toLowerCase(Locale.ROOT)
    println(lowerCase2) // sortie: hello world
}
```

Comme vous pouvez le constater, les deux méthodes produisent le même résultat. La seule différence est que la méthode `toLowerCase(Locale)` vous permet de spécifier une locale, qui peut être utile si vous avez besoin de tenir compte des règles de casse spécifiques à une langue ou une région donnée.

## Plongée En Profondeur
La fonction `toLowerCase()` et la méthode `toLowerCase(Locale)` utilisent toutes deux le sous-système de casse de Kotlin, qui repose sur le standard Unicode. Ce système prend en charge le cassement de toutes les langues dans le monde, ce qui signifie que la méthode `toLowerCase()` peut gérer correctement les majuscules et les caractères spéciaux de toutes les langues. En outre, la méthode `toLowerCase()` et la fonction `toLowerCase()` sont toutes deux des fonctions d'extension. Cela signifie que vous pouvez également les utiliser sur des types de données personnalisés, à condition qu'ils aient une méthode `toString()`.

## Voir Aussi
- [Documentation de la bibliothèque standard de Kotlin](https://kotlinlang.org/api/latest/jvm/stdlib/kotlin.text/to-lower-case.html)
- [Guide Unicode de Kotlin](https://kotlinlang.org/docs/reference/native/encoding-and-decoding.html#unicode)