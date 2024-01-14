---
title:                "Kotlin: Trouver la longueur d'une chaîne"
programming_language: "Kotlin"
category:             "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fr/kotlin/finding-the-length-of-a-string.md"
---

{{< edit_this_page >}}

## Pourquoi

L'une des tâches les plus courantes en programmation est de trouver la longueur d'une chaîne de caractères. Cela peut sembler simple, mais comprendre comment le faire correctement peut faire une grande différence dans la performance de votre application.

## Comment faire

Pour trouver la longueur d'une chaîne en Kotlin, vous pouvez utiliser la méthode `length()` qui est disponible sur tous les objets String. Voyons un exemple concret :

```Kotlin
val chaine = "Bonjour!"
println(chaine.length()) // Sortie : 8
```

Comme vous pouvez le voir, il suffit d'appeler la méthode `length()` sur la chaîne et cela vous renvoie le nombre total de caractères. Mais il est important de noter que la méthode `length()` compte également les espaces et la ponctuation, ce qui peut affecter votre résultat si vous ne les prenez pas en compte dans votre logique.

## Approfondissement

Il est également important de comprendre comment la longueur d'une chaîne est calculée en Kotlin. En interne, Kotlin utilise la propriété `length` qui retourne directement la taille de la chaîne, sans effectuer de calcul supplémentaire.

Il est également possible de parcourir une chaîne en utilisant une boucle `for` et en utilisant la méthode `indices` pour obtenir les index numériques de chaque caractère. Voyons un exemple :

```Kotlin
val chaine = "Kotlin"
for (i in chaine.indices) {
    println(chaine[i]) // Sortie : K o t l i n
}
```

Ceci nous permet de récupérer chaque caractère individuellement et de les utiliser dans notre code. Il est important de noter que les indices commencent à zéro en Kotlin.

## See Also

- [Documentation Kotlin sur les chaînes](https://kotlinlang.org/docs/basic-types.html#strings)
- [Comparaison de la performance de la longueur des chaînes en Kotlin](https://blog.kotlin-academy.com/kotlin-performance-benefits-ofstring-length-in-android-6771e59640ba)

Merci d'avoir lu cet article sur la façon de trouver la longueur d'une chaîne en Kotlin ! Espérons que cela vous a donné une meilleure compréhension de cette tâche courante en programmation. N'hésitez pas à explorer les liens ci-dessus pour en savoir plus sur les chaînes en Kotlin. À bientôt !