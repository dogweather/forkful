---
date: 2024-01-26 03:45:20.807570-07:00
description: "Wie: In Kotlin kann das Runden mithilfe mehrerer Funktionen wie `roundToInt()`,\
  \ `roundToDouble()` und unter Verwendung von `BigDecimal` f\xFCr mehr Kontrolle\u2026"
lastmod: '2024-03-13T22:44:53.841860-06:00'
model: gpt-4-0125-preview
summary: "In Kotlin kann das Runden mithilfe mehrerer Funktionen wie `roundToInt()`,\
  \ `roundToDouble()` und unter Verwendung von `BigDecimal` f\xFCr mehr Kontrolle\
  \ erfolgen."
title: Zahlen runden
weight: 13
---

## Wie:
In Kotlin kann das Runden mithilfe mehrerer Funktionen wie `roundToInt()`, `roundToDouble()` und unter Verwendung von `BigDecimal` für mehr Kontrolle erfolgen:

```kotlin
fun main() {
    val number1 = 3.14159
    println(number1.roundToInt()) // Gibt aus: 3

    val number2 = 3.5
    println(number2.roundToInt()) // Gibt aus: 4

    val number3 = 123.456
    println("%.2f".format(number3)) // Gibt aus: 123.46
    
    val bigDecimal = number3.toBigDecimal().setScale(1, RoundingMode.HALF_EVEN)
    println(bigDecimal) // Gibt aus: 123.5
}
```

## Vertiefende Betrachtung
Historisch gesehen ist das Runden von Zahlen ein grundlegendes Konzept sowohl in der Mathematik als auch in der Informatik, konzipiert, um mit den Einschränkungen der numerischen Präzision umzugehen. In der frühen Datenverarbeitung war das Runden aufgrund der hohen Speicherkosten von entscheidender Bedeutung.

In Kotlin basiert das Runden auf den Standard-Java-Bibliotheken. Optionen für das Runden beinhalten `Math.round()`, welches zur nächsten ganzen Zahl rundet, und `BigDecimal` für anpassbares Runden, wo man einen Skalierungsgrad und einen `RoundingMode` spezifizieren kann. 

Jeder `RoundingMode` hat unterschiedliche Richtlinien für den Umgang mit Unentschiedenheiten (wenn die Ziffer genau in der Mitte der Optionen zum Runden steht). Zum Beispiel rundet `RoundingMode.HALF_UP` zur nächstliegenden Nachbarzahl, es sei denn, beide Nachbarn sind gleich entfernt, in welchem Fall es aufrundet.

## Siehe auch
- Kotlin Dokumentation zu [`BigDecimal`](https://kotlinlang.org/api/latest/jvm/stdlib/kotlin/java.math.-big-decimal/index.html)
- Oracles Java-Dokumentation für [`RoundingMode`](https://docs.oracle.com/javase/8/docs/api/java/math/RoundingMode.html)
- IEEE-Standard für Gleitkommaarithmetik (IEEE 754) [IEEE Standard 754](https://ieeexplore.ieee.org/document/4610935)
