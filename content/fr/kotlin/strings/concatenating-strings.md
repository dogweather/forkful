---
date: 2024-01-20 17:35:06.076480-07:00
description: "Concat\xE9ner des cha\xEEnes de caract\xE8res, c'est les joindre bout\
  \ \xE0 bout. On le fait pour cr\xE9er des textes dynamiques ou assembler des bouts\
  \ d'info."
lastmod: '2024-03-13T22:44:57.728904-06:00'
model: gpt-4-1106-preview
summary: "Concat\xE9ner des cha\xEEnes de caract\xE8res, c'est les joindre bout \xE0\
  \ bout. On le fait pour cr\xE9er des textes dynamiques ou assembler des bouts d'info."
title: "Concat\xE9nation de cha\xEEnes de caract\xE8res"
weight: 3
---

## What & Why?
Concaténer des chaînes de caractères, c'est les joindre bout à bout. On le fait pour créer des textes dynamiques ou assembler des bouts d'info.

## How to:
```Kotlin
fun main() {
    val hello = "Bonjour"
    val world = "le monde"
    val exclamation = "!"

    // Utilisation du '+'
    val greeting1 = hello + ", " + world + exclamation
    println(greeting1) // Affiche: Bonjour, le monde!

    // String templates avec '$'
    val greeting2 = "$hello, $world$exclamation"
    println(greeting2) // Affiche: Bonjour, le monde!

    // StringBuilder pour des concaténations multiples
    val builder = StringBuilder()
    builder.append(hello)
    builder.append(", ")
    builder.append(world)
    builder.append(exclamation)
    val greeting3 = builder.toString()
    println(greeting3) // Affiche: Bonjour, le monde!
}
```

## Deep Dive
Au début, les développeurs concaténaient les chaînes avec l'opérateur `+`. C'était simple mais pas très efficace pour de nombreux ajouts. Kotlin a introduit les templates de chaînes pour rendre le code plus lisible et plus concis. Pour des concaténations en boucle ou complexes, on préfère utiliser `StringBuilder` pour une meilleure performance.

Alternativement, on peut exploiter `joinToString` si on travaille avec des collections:

```Kotlin
val words = listOf("Bonjour", "le", "monde")
val greeting = words.joinToString(separator = " ", postfix = "!") 
// Bonjour le monde!
```
`joinToString` est plus expressif et évite de gérer l'ajout manuel de séparateurs.

## See Also
- Kotlin documentation: [Basic Types](https://kotlinlang.org/docs/basic-types.html)
- Kotlin style guide: [Coding Conventions](https://kotlinlang.org/docs/coding-conventions.html#string-templates)
