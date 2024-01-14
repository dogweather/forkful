---
title:                "Kotlin: Recherche et remplacement de texte"
programming_language: "Kotlin"
category:             "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fr/kotlin/searching-and-replacing-text.md"
---

{{< edit_this_page >}}

## Pourquoi

La recherche et le remplacement de texte sont des tâches courantes en programmation. Cela peut être utile pour remplacer des mots mal orthographiés, mettre à jour du code existant ou encore pour automatiser des modifications massives dans un fichier de données.

## Comment faire

La bibliothèque standard de Kotlin offre une méthode pratique pour effectuer une recherche et un remplacement de texte. Voici un exemple de code pour remplacer toutes les occurrences de "bonjour" par "salut" dans une chaîne de caractères :

```Kotlin
val maChaine = "Bonjour tout le monde !"
val maNouvelleChaine = maChaine.replace("bonjour", "salut")
println(maNouvelleChaine)
```

Le résultat affichera "Salut tout le monde !". Comme vous pouvez le voir, la méthode `replace` prend en premier paramètre le texte à remplacer et en deuxième paramètre le nouveau texte.

Nous pouvons également utiliser des expressions régulières pour effectuer une recherche et un remplacement plus complexe. Par exemple, si nous voulons remplacer le mot "chat" par "chien" uniquement s'il est suivi d'un espace ou d'un point d'exclamation, nous pouvons utiliser cette expression régulière `(?<=\\s|!)chat`.

```Kotlin
val maChaine = "J'aime mon chat !"
val expresReg = Regex("(?<=\\s|!)chat")
val maNouvelleChaine = expresReg.replace(maChaine, "chien")
println(maNouvelleChaine)
```

Le résultat affichera "J'aime mon chien !".

## Plongée en profondeur

En plus de la méthode `replace` de la bibliothèque standard, Kotlin offre également une fonction `replace` qui peut être appelée sur des chaînes de caractères. Cela signifie que nous pouvons écrire `maChaine.replace(...)` ou simplement `maChaine.replace(...)`.

De plus, lors de l'utilisation d'expressions régulières, il est possible d'utiliser la méthode `toRegex()` pour créer un objet de type `Regex` à partir d'une chaîne de caractères.

Il est également important de noter que les méthodes de recherche et de remplacement en Kotlin sont sensibles à la casse par défaut. Pour rendre la recherche et le remplacement insensibles à la casse, nous pouvons utiliser l'option `IGNORE_CASE`.

Enfin, nous pouvons également utiliser la méthode `replaceFirst` pour remplacer uniquement la première occurrence trouvée.

## Voir aussi

- La documentation officielle de Kotlin sur les méthodes de recherche et de remplacement : https://kotlinlang.org/api/latest/jvm/stdlib/kotlin.text/replace.html
- Un tutoriel complet sur l'utilisation des expressions régulières en Kotlin : https://www.geeksforgeeks.org/kotlin-regex-class-and-its-methods/

Merci d'avoir lu cet article sur la recherche et le remplacement de texte en Kotlin ! N'hésitez pas à explorer davantage les fonctionnalités de la bibliothèque standard pour trouver de nombreuses autres options intéressantes. À bientôt !