---
date: 2024-01-20 17:47:52.257249-07:00
description: "Trouver la longueur d'une cha\xEEne de caract\xE8res c\u2019est d\xE9\
  couvrir le nombre de caract\xE8res qu'elle contient. Les programmeurs font cela\
  \ pour valider des\u2026"
lastmod: '2024-03-13T22:44:57.727379-06:00'
model: gpt-4-1106-preview
summary: "Trouver la longueur d'une cha\xEEne de caract\xE8res c\u2019est d\xE9couvrir\
  \ le nombre de caract\xE8res qu'elle contient."
title: "Trouver la longueur d'une cha\xEEne de caract\xE8res"
weight: 7
---

## How to:
En Kotlin, utilisez la propriété `length` pour obtenir la longueur d'une chaîne.

```kotlin
fun main() {
    val greeting = "Bonjour!"
    println("La longueur de la chaîne est : ${greeting.length}")
}
```
Sample output:
```
La longueur de la chaîne est : 8
```

## Deep Dive
Historiquement, la longueur des chaînes a toujours été cruciale pour la manipulation des textes. Kotlin simplifie cette opération avec la propriété `length`. Autrefois, des langages comme C requéraient des fonctions (comme `strlen()`) pour parcourir la chaîne jusqu'au caractère null final.

Alternatives:
- Boucler sur chaque caractère (pas efficace).
- Utiliser des méthodes Kotlin pour les collections, comme `count()`, qui peuvent servir à des cas plus spécifiques.

Détails d'implémentation:
La longueur d'une chaîne en Kotlin compte les unités de code UTF-16, pas forcément les caractères Unicode individuels. Pour des chaînes avec des caractères hors du Plan Multilingue de Base (BMP), ça peut être trompeur. Exemple, un émoji tel que 🧡 est composé de deux unités de code UTF-16.

```kotlin
val heartEmoji = "🧡"
println("Longueur UTF-16 : ${heartEmoji.length}") // Output: 2
```

## See Also
- Kotlin Documentation: [Basic Types](https://kotlinlang.org/docs/basic-types.html#strings)
- Unicode Consortium: [FAQ - UTF-8, UTF-16, UTF-32 & BOM](https://unicode.org/faq/utf_bom.html)
- Oracle Java Tutorial: [String Length](https://docs.oracle.com/javase/tutorial/java/data/strings.html) (Je sais que ce n'est pas Kotlin, mais il donne du contexte sur la manipulation des chaînes en Java, qui influence Kotlin.)
