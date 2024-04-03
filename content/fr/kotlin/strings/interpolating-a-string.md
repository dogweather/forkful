---
date: 2024-01-20 17:51:00.270311-07:00
description: "How to: Kotlin rend l'interpolation de cha\xEEnes simple comme bonjour.\
  \ Tu as juste \xE0 placer un dollar `$` suivi par le nom de la variable."
lastmod: '2024-03-13T22:44:57.721602-06:00'
model: gpt-4-1106-preview
summary: "Kotlin rend l'interpolation de cha\xEEnes simple comme bonjour."
title: "Interpolation de cha\xEEnes de caract\xE8res"
weight: 8
---

## How to:
Kotlin rend l'interpolation de chaînes simple comme bonjour. Tu as juste à placer un dollar `$` suivi par le nom de la variable.

```kotlin
fun main() {
    val prénom = "Claude"
    println("Salut, $prénom ! Comment ça va ?")
    // Affiche: Salut, Claude ! Comment ça va ?

    val age = 30
    println("Tu as $age ans, donc tu es né en ${2023 - age}.")
    // Affiche: Tu as 30 ans, donc tu es né en 1993.
}
```

## Deep Dive
L'interpolation de chaînes n'est pas nouvelle. Elle date des premiers langages de script comme Perl ou Bash. En Kotlin, elle est propre et intégrée directement dans le langage, évitant la concaténation fastidieuse avec `+`. Kotlin compile ces interpolations en code bytecode efficace, tout comme s'il s'agissait d'une concaténation simple. Mais attention, abuser des interpolations complexes peut réduire la lisibilité. Le bon usage est la clé.

## See Also
Si tu veux creuser un peu plus, voilà des ressources utiles :

- La documentation officielle Kotlin sur les chaînes de caractères : [Documentation Kotlin](https://kotlinlang.org/docs/basic-types.html#strings)
- Un guide complet pour maîtriser l'interpolation de chaînes en Kotlin : [Baeldung Kotlin String Interpolation](https://www.baeldung.com/kotlin/string-interpolation)
