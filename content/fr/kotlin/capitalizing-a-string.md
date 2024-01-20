---
title:                "Mettre une chaîne en majuscules"
html_title:           "Kotlin: Mettre une chaîne en majuscules"
simple_title:         "Mettre une chaîne en majuscules"
programming_language: "Kotlin"
category:             "Kotlin"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fr/kotlin/capitalizing-a-string.md"
---

{{< edit_this_page >}}

## Qu'est-ce que c'est & Pourquoi ? 

La mise en majuscule d'une chaîne consiste à transformer les premières lettres de chaque mot en majuscules. Les programmeurs le font souvent pour améliorer la lisibilité et la présentation des éléments textuels dans l'interface utilisateur.

## Comment faire:

En Kotlin, vous pouvez utiliser la fonction `capitalize()` pour mettre en majuscule une chaîne.
```Kotlin
val maChaine = "bonjour tout le monde"
val resultat = maChaine.split(" ").joinToString(" ") { it.capitalize() }
println(resultat)
```
Sortie échantillon
```Kotlin
Bonjour Tout Le Monde
```

## Plongée en profondeur

1. _Contexte historique_ : À l'origine, les ordinateurs ne distinguaient pas les caractères minuscules des majuscules. Avec l'évolution de la technologie, la distinction entre les deux est devenue possible, améliorant ainsi la lisibilité et l'efficacité du traitement des chaînes.

2. _Alternatives_ : En Kotlin, vous pouvez également utiliser la fonction `toUpperCase` pour convertir tous les caractères d'une chaîne en majuscules. Cependant, `capitalize` est préférée lorsque vous ne voulez mettre en majuscule que le premier caractère de chaque mot.

3. _Détails de mise en œuvre_ : La fonction `capitalize` de Kotlin utilise l'algorithme Unicode pour déterminer quels caractères doivent être convertis en majuscules. Cela permet de gérer correctement même les langues non latines.

## Voir aussi

2. Guide de l'utilisateur Kotlin sur la gestion des chaînes : [https://kotlinlang.org/docs/basic-types.html#string-literals](https://kotlinlang.org/docs/basic-types.html#string-literals)
3. Autres techniques de manipulation de chaînes en Kotlin : [https://www.programiz.com/kotlin-programming/string](https://www.programiz.com/kotlin-programming/string)