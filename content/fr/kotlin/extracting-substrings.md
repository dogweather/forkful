---
title:                "Kotlin: Extraction de sous-chaînes"
simple_title:         "Extraction de sous-chaînes"
programming_language: "Kotlin"
category:             "Kotlin"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fr/kotlin/extracting-substrings.md"
---

{{< edit_this_page >}}

# Pourquoi

L'extraction de sous-chaînes, également connue sous le nom de slicing en anglais, est une méthode utile pour manipuler des chaînes de caractères en Kotlin. Elle permet de découper une chaîne en parties plus petites, ce qui peut être très utile dans différentes situations de programmation.

# Comment faire

Pour extraire une sous-chaîne à partir d'une chaîne existante en Kotlin, vous pouvez utiliser la fonction `subSequence()` ou l'opérateur `..`.

Voici un exemple utilisant la fonction `subSequence()` :

```Kotlin
val chaine = "Bonjour tout le monde"
val sousChaine = chaine.subSequence(0, 7)
println(sousChaine) // affiche "Bonjour"
```

Et voici un exemple utilisant l'opérateur `..` :

```Kotlin
val chaine = "Bonjour tout le monde"
val sousChaine = chaine[8..12]
println(sousChaine) //affiche "tout"
```

# Plongée en profondeur

Il est important de noter que la fonction `subSequence()` retourne une instance de `CharSequence`, qui est une interface immuable. Si vous souhaitez manipuler la sous-chaîne en tant que chaîne de caractères mutable, vous devez la convertir en utilisant la fonction `toString()`.

Il est également possible d'utiliser des nombres négatifs pour extraire une sous-chaîne en partant de la fin de la chaîne. Par exemple, `chaine[-5..-1]` retournera une sous-chaîne des 5 derniers caractères de la chaîne.

Enfin, vous pouvez également utiliser la fonction `substring()` pour extraire une sous-chaîne à partir d'un index donné jusqu'à la fin de la chaîne, sans avoir à spécifier la longueur de la sous-chaîne.

# Voir aussi

Voici quelques liens utiles pour en apprendre davantage sur l'extraction de sous-chaînes en Kotlin :

- Documentation officielle de Kotlin sur les chaînes de caractères : https://kotlinlang.org/docs/reference/basic-types.html#strings
- Tutoriel vidéo sur l'extraction de sous-chaînes en Kotlin : https://www.youtube.com/watch?v=hqpzzZJKqW4
- Exemples de code pour pratiquer l'extraction de sous-chaînes en Kotlin : https://www.programiz.com/kotlin-programming/strings#substring