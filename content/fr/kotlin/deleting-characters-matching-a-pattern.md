---
title:    "Kotlin: Suppression de caractères correspondant à un motif"
keywords: ["Kotlin"]
---

{{< edit_this_page >}}

## Pourquoi

Supprimer des caractères correspondant à un motif peut être une tâche courante en programmation, surtout lorsque l'on travaille avec des chaînes de caractères. Cette action peut être utile pour nettoyer des données ou pour effectuer une vérification de formulaire. Dans cet article, nous allons explorer différentes façons de supprimer des caractères correspondant à un motif en utilisant Kotlin.

## Comment faire

Pour supprimer des caractères correspondant à un motif en Kotlin, nous pouvons utiliser la méthode `replace` de la classe `String`. Cette méthode prend en paramètre une expression régulière qui spécifie le motif à supprimer et une chaîne de caractères de remplacement. Voyons un exemple concret :

```Kotlin
val str = "Hello World!"
val newStr = str.replace(Regex("[a-z]"), "")
println(newStr) // Output: H W!
```

Dans cet exemple, nous utilisons une expression régulière `[a-z]` qui correspond à toutes les lettres minuscules de l'alphabet. Nous la remplaçons par une chaîne vide, ce qui a pour effet de supprimer toutes les lettres minuscules de notre chaîne de caractères `str`.

Il est également possible d'utiliser la méthode `replaceFirst` pour supprimer uniquement la première occurrence du motif dans la chaîne :

```Kotlin
val str = "Kotlin peut être amusant!"
val newStr = str.replaceFirst(Regex("am"), "")
println(newStr) // Output: K