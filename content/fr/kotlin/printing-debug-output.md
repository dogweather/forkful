---
title:    "Kotlin: Affichage des sorties de débogage"
keywords: ["Kotlin"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/fr/kotlin/printing-debug-output.md"
---

{{< edit_this_page >}}

## Pourquoi

Il y a plusieurs raisons pour lesquelles un développeur pourrait utiliser l'impression de sortie de débogage dans un programme Kotlin. Tout d'abord, cela peut être utile pour comprendre le comportement de votre code et trouver des erreurs. De plus, cela peut également être une méthode simple pour vérifier si certaines valeurs sont correctement assignées ou pour suivre le flux d'exécution du programme.

## Comment faire

Pour imprimer une sortie de débogage dans Kotlin, vous pouvez utiliser la fonction `println()` comme ceci:

```Kotlin
val variable = 10
println("La valeur de la variable est : $variable")
```

Cela imprimera "La valeur de la variable est : 10" dans la console de votre IDE.

De plus, Kotlin offre également la possibilité d'utiliser l'opérateur d'interpolation de chaîne pour donner des informations plus détaillées dans votre sortie de débogage. Voici un exemple:

```Kotlin
val prenom = "Marie"
val age = 25

println("$prenom a $age ans.")
```

Cela affichera "Marie a 25 ans" dans la console. Vous pouvez même utiliser des expressions plus complexes à l'intérieur des accolades pour imprimer des valeurs calculées.

## Plongée en profondeur

L'impression de sortie de débogage peut également être utilisée pour faciliter le processus de test de votre code. Vous pouvez utiliser des instructions `println()` à différentes étapes de votre programme pour vérifier si les valeurs sont correctement assignées et si le flux d'exécution suit le chemin prévu.

De plus, Kotlin offre un moyen pratique de désactiver rapidement toutes les instructions de débogage dans votre code en ajoutant le préfixe `@Composable` avant votre fonction principale, comme ceci:

```Kotlin
@Composable
fun main() {
    // Votre code ici
}
```

Cela évite que vos instructions de débogage n'affectent les performances de votre programme en production.

## Voir aussi

- [Documentation officielle Kotlin sur la sortie de débogage](https://kotlinlang.org/docs/tutorials/kotlin-for-py/destructuring-declarations.html#namespace)
- [Article sur l'impression de débogage en Kotlin](https://dev.to/martinpham/logging-in-kotlin-3npd) (en anglais)
- [Vidéo tutoriel sur l'utilisation de l'impression de débogage en Kotlin](https://www.youtube.com/watch?v=-UruWPAV4v8) (en anglais)