---
changelog:
- 2024-02-03, gpt-4-0125-preview, translated from English
date: 2024-02-03 19:05:38.354320-07:00
description: "Comment faire : En Kotlin, les cha\xEEnes de caract\xE8res peuvent \xEA\
  tre mises en majuscules en utilisant les fonctions de la biblioth\xE8que standard\
  \ sans\u2026"
lastmod: '2024-03-13T22:44:57.717727-06:00'
model: gpt-4-0125-preview
summary: "En Kotlin, les cha\xEEnes de caract\xE8res peuvent \xEAtre mises en majuscules\
  \ en utilisant les fonctions de la biblioth\xE8que standard sans n\xE9cessiter de\
  \ biblioth\xE8ques tierces."
title: "Mettre en majuscule une cha\xEEne"
weight: 2
---

## Comment faire :
En Kotlin, les chaînes de caractères peuvent être mises en majuscules en utilisant les fonctions de la bibliothèque standard sans nécessiter de bibliothèques tierces. L'approche de Kotlin pour manipuler les chaînes rend ces opérations simples et concises.

### Mettre toute la chaîne en majuscules :
```kotlin
val message = "hello, world!"
val capitalizedMessage = message.uppercase()

println(capitalizedMessage) // Sortie : HELLO, WORLD!
```

### Mettre en majuscule uniquement le premier caractère :
Depuis Kotlin 1.5, la fonction `capitalize()` est obsolète et remplacée par une combinaison de `replaceFirstChar` et d'une lambda qui vérifie s'il s'agit d'une lettre minuscule pour la transformer en majuscule.

```kotlin
val greeting = "hello, world!"
val capitalizedGreeting = greeting.replaceFirstChar {
    if (it.isLowerCase()) it.titlecase() else it.toString()
}

println(capitalizedGreeting) // Sortie : Hello, world!
```

Cette approche maintient le reste de la phrase dans sa forme originale tout en changeant uniquement la première lettre en majuscule.
