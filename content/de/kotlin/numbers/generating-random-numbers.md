---
title:                "Generierung von Zufallszahlen"
aliases:
- /de/kotlin/generating-random-numbers.md
date:                  2024-01-27T20:34:25.182879-07:00
model:                 gpt-4-0125-preview
simple_title:         "Generierung von Zufallszahlen"

tag:                  "Numbers"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/de/kotlin/generating-random-numbers.md"
---

{{< edit_this_page >}}

## Was & Warum?

Das Erzeugen von Zufallszahlen in der Programmierung bedeutet, Zahlen zu schaffen, die kein vorhersagbares Muster aufweisen. Programmierer tun dies aus verschiedenen Gründen, einschließlich Simulationen, Algorithmus-Tests, Spielen und Sicherheitsanwendungen, bei denen Unvorhersehbarkeit der Schlüssel zu realistischen oder sicheren Ergebnissen ist.

## Wie:

Kotlin bietet eine unkomplizierte Möglichkeit, Zufallszahlen über seine Standardbibliothek zu generieren. Hier erfahren Sie, wie Sie verschiedene Arten von Zufallswerten erzeugen können:

### Erzeugen einer zufälligen Ganzzahl

Um eine zufällige Ganzzahl in einem bestimmten Bereich zu generieren:

```kotlin
import kotlin.random.Random

fun main() {
    val randomNumber = Random.nextInt(1, 100) // Erzeugt eine zufällige Zahl zwischen 1 und 99
    println(randomNumber)
}
```

### Erzeugen einer zufälligen Gleitkommazahl

Ähnlich verhält es sich mit der Erzeugung einer zufälligen Gleitkommazahl:

```kotlin
import kotlin.random.Random

fun main() {
    val randomDouble = Random.nextDouble(1.0, 10.0) // Erzeugt eine zufällige Gleitkommazahl zwischen 1.0 und 10.0
    println(randomDouble)
}
```

### Erzeugen eines zufälligen Booleschen Wertes

Um einen zufälligen booleschen Wert zu generieren:

```kotlin
import kotlin.random.Random

fun main() {
    val randomBoolean = Random.nextBoolean() // Erzeugt zufällig entweder true oder false
    println(randomBoolean)
}
```

### Setzen eines Seeds für reproduzierbare Ergebnisse

In Fällen, in denen Sie reproduzierbare Folgen von Zufallszahlen benötigen (zum Beispiel beim Testen), können Sie den Zufallszahlengenerator mit einem Seed initialisieren:

```kotlin
import kotlin.random.Random

fun main() {
    val seed = 12345L
    val random = Random(seed)
    val randomNumber = random.nextInt(1, 100)
    println(randomNumber)
}
```

## Tiefgehender Einblick

Die Herangehensweise der Kotlin-Standardbibliothek an die Erzeugung von Zufallszahlen nutzt unter der Haube `java.util.Random` von Java, was eine Mischung aus Benutzerfreundlichkeit und Leistung sicherstellt. Es ist jedoch entscheidend zu beachten, dass diese Methoden pseudo-zufällige Zahlen erzeugen, was bedeutet, dass die Zahlen zwar zufällig erscheinen, aber mit einem deterministischen Prozess erzeugt werden.

Für die meisten Anwendungen ist die durch die `Random`-Klasse von Kotlin bereitgestellte Zufälligkeit ausreichend. Jedoch sollten für sicherheitssensitive Anwendungen, wie Kryptographie, bei denen die Qualität der Zufälligkeit von größter Bedeutung ist, `java.security.SecureRandom` in Betracht gezogen werden. SecureRandom ist speziell für kryptografische Operationen konzipiert und bietet eine höhere Qualität der Zufälligkeit, allerdings möglicherweise mit einem Leistungskompromiss.

Kotlin erfindet das Rad nicht neu, sondern bietet eine Kotlin-freundliche API über die Mechanismen der Zufallszahlengenerierung von Java, was deren Nutzung innerhalb von Kotlin-Projekten idiomatischer und knapper macht. Wie immer, wenn es um Zufälligkeit geht, sollten Programmierer den Anwendungsfall sorgfältig berücksichtigen, um das am besten geeignete Werkzeug für die Aufgabe auszuwählen.
