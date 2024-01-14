---
title:    "Kotlin: Extraction de sous-chaînes"
keywords: ["Kotlin"]
---

{{< edit_this_page >}}

# Pourquoi Extraire des Sous-Chaînes?

Extraire des sous-chaînes est un moyen utile de prendre une partie d'une chaîne de caractères et de la manipuler selon nos besoins. Cela peut être utile pour effectuer des opérations de recherche et de remplacement, ou pour obtenir une partie spécifique d'une chaîne plus longue. Dans cet article, nous allons explorer comment extraire des sous-chaînes en utilisant le langage de programmation Kotlin. 

## Comment Faire?

Pour extraire une sous-chaîne en Kotlin, nous allons utiliser la fonction `substring()` de la classe `String`. Cette fonction prend deux paramètres, `startIndex` et `endIndex`, qui déterminent quelle partie de la chaîne doit être extraite. Voici un exemple de code qui utilise `substring()`:

```Kotlin
val chaine = "Bonjour tout le monde!"
val sousChaine = chaine.substring(8, 13)
println(sousChaine)
```

Ce code va extraire la sous-chaîne "tout" de la chaîne originale et l'afficher dans la console. Vous remarquerez que l'index de la première lettre est 0, donc nous passons 8 comme `startIndex` et 13 comme `endIndex` pour obtenir la partie souhaitée de la chaîne.

Il est également possible de ne spécifier qu'un seul paramètre dans `substring()`, dans ce cas la sous-chaîne sera extraite à partir de l'index donné jusqu'à la fin de la chaîne. Par exemple:

```Kotlin
val chaine = "Bonjour tout le monde!"
val sousChaine = chaine.substring(8)
println(sousChaine)
```

Cela va extraire la sous-chaîne "tout le monde!" de la chaîne originale.

Il est important de noter que `startIndex` et `endIndex` sont inclus dans la sous-chaîne extraite. De plus, si les index spécifiés sont en dehors des limites de la chaîne, une erreur sera générée.

## Plongée Profonde

En plus de la fonction `substring()`, il existe d'autres méthodes et fonctions utiles pour la manipulation de sous-chaînes en Kotlin. Par exemple, il y a la fonction `subSequence()` qui fonctionne de la même manière que `substring()`, mais renvoie un objet de type `CharSequence` plutôt qu'une chaîne de caractères. Cela peut être utile si vous avez besoin d'un type de données différent pour travailler avec la sous-chaîne.

Il y a aussi la fonction `take()` qui peut être utilisée pour extraire un nombre spécifique de caractères à partir du début de la chaîne. Par exemple:

```Kotlin
val chaine = "Bonjour tout le monde!"
val sousChaine = chaine.take(7)
println(sousChaine)
```

Cela va extraire la sous-chaîne "Bonjour" de la chaîne originale. De plus, il y a la fonction `drop()` qui peut être utilisée pour supprimer un nombre spécifique de caractères à partir de la fin de la chaîne.

## Voir Aussi

- [Documentation officielle de la classe String dans Kotlin](https://kotlinlang.org/api/latest/jvm/stdlib/kotlin/-string/)
- [Guide de démarrage avec Kotlin pour les débutants](https://www.raywenderlich.com/235-kotlin-tutorial-for-android-getting-started)
- [Tutoriel de formation Kotlin sur Codecademy](https://www.codecademy.com/fr/learn/learn-kotlin)