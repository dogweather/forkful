---
title:                "Generierung von Zufallszahlen"
date:                  2024-01-20T17:49:32.738080-07:00
model:                 gpt-4-1106-preview
simple_title:         "Generierung von Zufallszahlen"
programming_language: "Kotlin"
category:             "Kotlin"
tag:                  "Numbers"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/de/kotlin/generating-random-numbers.md"
---

{{< edit_this_page >}}

## Was & Warum?
Zufallszahlen zu generieren heißt, nicht vorhersehbare Werte zu erzeugen - Programmierer nutzen sie für alles Mögliche, sei es für Spiele, Simulationen oder Sicherheitsfunktionen.

## How to:
```kotlin
import kotlin.random.Random

fun main() {
    val randomNumber = Random.nextInt(0, 100) // Zahlen zwischen 0 und 99
    println(randomNumber)
}
```
Ausgabe könnte sein: `42` (aber das variiert natürlich)

```kotlin
import kotlin.random.Random

fun main() {
    val randomDouble = Random.nextDouble(1.0, 10.0) // Nachkommazahlen zwischen 1.0 und 10.0
    println(randomDouble)
}
```
Ausgabe könnte sein: `4.2035237673` (aber das variiert natürlich)

## Deep Dive
Früher nutzten Programmierer oft die Systemzeit als Basis für Zufallszahlen, aber moderne Ansätze wie Kotlins `Random`-Klasse bieten bessere Verteilung und Unvorhersehbarkeit. Es gibt Alternativen wie `java.util.Random` oder `ThreadLocalRandom`, aber in Kotlin ist `kotlin.random.Random` besonders praktisch, da es direkt und ohne weiteres Einrichten genutzt werden kann. `Random` generiert Pseudozufallszahlen, was bedeutet, dass sie durch Algorithmen erzeugt werden und streng genommen nicht wirklich zufällig sind, aber für die meisten Anwendungsfälle mehr als ausreichend.

## See Also
- Offizielle Kotlin-Dokumentation über Random: [https://kotlinlang.org/api/latest/jvm/stdlib/kotlin.random/-random/](https://kotlinlang.org/api/latest/jvm/stdlib/kotlin.random/-random/)
- Wikipedia-Artikel über Pseudozufallszahlengeneratoren: [https://de.wikipedia.org/wiki/Pseudozufallszahlengenerator](https://de.wikipedia.org/wiki/Pseudozufallszahlengenerator)
- Mehr zu Random und Thread-Sicherheit: [https://docs.oracle.com/javase/8/docs/api/java/util/concurrent/ThreadLocalRandom.html](https://docs.oracle.com/javase/8/docs/api/java/util/concurrent/ThreadLocalRandom.html)
