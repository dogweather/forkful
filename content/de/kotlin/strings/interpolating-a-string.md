---
date: 2024-01-20 17:51:09.625047-07:00
description: 'So geht''s: .'
lastmod: '2024-03-13T22:44:53.833583-06:00'
model: gpt-4-1106-preview
summary: .
title: Zeichenketten interpolieren
weight: 8
---

## So geht's:
```kotlin
fun main() {
    val name = "Welt"
    val greeting = "Hallo, $name!"
    println(greeting)  // Gibt aus: Hallo, Welt!

    val eins = 1
    val meldung = "Eins plus eins ergibt ${eins + eins}"
    println(meldung)  // Gibt aus: Eins plus eins ergibt 2
}
```

## Tiefgang:
Historisch gesehen entstanden viele Programmiersprachen ohne String-Interpolation, was zur Verwendung von umständlichen Methoden wie String-Konkatenation führte. Kotlin, inspiriert von modernen Sprachen wie Ruby und Swift, hat String-Interpolation von Anfang an eingebunden. Diese erleichtert nicht nur die String-Verwaltung, sondern macht den Code auch sicherer, da es weniger Fehlerquellen, wie falsch gesetzte Leerzeichen oder Vergessen von Variablen, gibt. Man kann zwar alternativ StringBuilder oder String.format verwenden, aber Interpolation ist oft klarer und effizienter.

## Siehe auch:
- [Kotlin Dokumentation zur String-Interpolation](https://kotlinlang.org/docs/basic-syntax.html#string-templates)
- [Kotlin Style Guide](https://developer.android.com/kotlin/style-guide)
- [Kotlin Playground](https://play.kotlinlang.org/) – zum Experimentieren mit Kotlin Code.
