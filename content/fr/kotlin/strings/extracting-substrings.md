---
date: 2024-01-20 17:46:13.974366-07:00
description: "Extraire des sous-cha\xEEnes, c'est simplement r\xE9cup\xE9rer une partie\
  \ d'une cha\xEEne de caract\xE8res. Les programmeurs le font pour analyser, manipuler\
  \ ou tester\u2026"
lastmod: '2024-03-13T22:44:57.725137-06:00'
model: gpt-4-1106-preview
summary: "Extraire des sous-cha\xEEnes, c'est simplement r\xE9cup\xE9rer une partie\
  \ d'une cha\xEEne de caract\xE8res."
title: "Extraction de sous-cha\xEEnes"
weight: 6
---

## How to:
Kotlin offre plusieurs façons d'extraire des sous-chaînes. Voici des exemples simples :

```kotlin
fun main() {
    val texte = "Salut le monde! Kotlin est génial."

    // Extrait à partir d'un index jusqu'à la fin
    val finTexte = texte.substring(14)
    println(finTexte) // Output: Kotlin est génial.

    // Extrait entre deux indices
    val milieuTexte = texte.substring(9, 14)
    println(milieuTexte) // Output: monde

    // Utilisation de ranges
    val rangeTexte = texte.substring(0..4)
    println(rangeTexte) // Output: Salut
}
```

## Deep Dive
Extraire des sous-chaînes est une pratique aussi vieille que la programmation elle-même. Avant Kotlin, Java utilisait `substring`. En Kotlin, les choses sont simplifiées avec des fonctions intégrées et l'utilisation de plages (`range`), rendant le code plus lisible.

Il existe d'autres alternatives comme:

- Les méthodes `take` et `drop` pour prendre ou supprimer un certain nombre de caractères depuis un bout de la chaîne.
- Les expressions régulières (`Regex`) pour des cas plus complexes.

Kotlin, étant typé statiquement, gère bien les index hors limites en lançant des exceptions, ce qui évite les erreurs subtiles lors de l'exécution.

## See Also
Pour aller plus loin, je vous suggère ces liens :

- Kotlin Stdlib pour plus de fonctions de manipulation de texte : [Kotlin stdlib](https://kotlinlang.org/api/latest/jvm/stdlib/kotlin.text/)
