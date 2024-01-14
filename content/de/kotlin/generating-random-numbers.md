---
title:                "Kotlin: Erzeugen von Zufallszahlen"
programming_language: "Kotlin"
category:             "Numbers"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/de/kotlin/generating-random-numbers.md"
---

{{< edit_this_page >}}

# Warum

In der Welt der Programmierung gibt es immer wieder Situationen, in denen wir zufällige Daten benötigen. Sei es für die Erstellung von Spielen, die Erzeugung von Testdaten oder die Implementierung von zufälligen Algorithmen. Das Generieren von Zufallszahlen ist dabei ein essentieller Bestandteil. Doch wie können wir in Kotlin solche Zufallszahlen erzeugen? In diesem Blogbeitrag zeige ich euch, wie einfach es ist, in Kotlin randomisierte Daten zu generieren.

# Wie man Zufallszahlen in Kotlin erzeugt

Die Generierung von Zufallszahlen in Kotlin basiert auf der Verwendung der Random-Klasse. Diese bietet verschiedene Methoden, um zufällige Daten in unterschiedlichen Datenstrukturen zu erzeugen. Schauen wir uns einige Beispiele an:

```
Kotlin

// Ganze Zahlen zwischen 1 und 100
val randomNumber = Random.nextInt(1,100)

// Dezimalzahlen zwischen 0 und 1
val randomDouble = Random.nextDouble()

// Boolesche Werte
val randomBoolean = Random.nextBoolean()

// Element aus einer Liste wählen
val fruits = listOf("Apfel", "Banane", "Orange")
val randomFruit = fruits.random()
```

Die `Random`-Klasse bietet auch die Möglichkeit, eigene Datenstrukturen zu erstellen und diese zufällig zu befüllen. Dafür müssen wir einen Generator definieren, der angibt, wie die zufälligen Daten erzeugt werden sollen. Ein Beispiel dafür wäre folgendes:

```
Kotlin
// Eigener Generator für die Erzeugung von Passwörtern
val passwordLength = 8
val generator = RandomGenerator(passwordLength) { charset ->
    val password = StringBuilder()
    repeat(passwordLength) {
        password.append(charset.random())
    }
    password.toString()
}

// Verwendung des Generators
val randomPassword = generator.generate()
```

# Tiefere Einblicke in die Generierung von Zufallszahlen

Die Generierung von Zufallszahlen basiert auf mathematischen Algorithmen, die dafür sorgen, dass die erzeugten Daten als zufällig empfunden werden. Dabei ist es wichtig zu verstehen, dass diese Zahlen nicht wirklich zufällig sind, sondern nur aufgrund bestimmter Berechnungen so erscheinen. Aus diesem Grund ist es auch wichtig, die `Random`-Klasse richtig zu verwenden und keine eigenen Algorithmen zum Generieren von Zufallszahlen zu implementieren.

Ein weiterer wichtiger Punkt ist die sogenannte Seed-Initialisierung. Diese wird verwendet, um den Startwert für die Generierung von Zufallszahlen festzulegen. Wird für jeden Aufruf der `Random`-Klasse eine neue Seed-Initialisierung vorgenommen, so werden auch immer neue Zufallszahlen erzeugt. Wird jedoch für jeden Aufruf die gleiche Seed-Initialisierung verwendet, so werden auch immer die gleichen Zufallszahlen erzeugt.

# Siehe auch

- [Kotlin Dokumentation zur Random-Klasse](https://kotlinlang.org/api/latest/jvm/stdlib/kotlin.random/-random/)
- [Erzeugen von Passwörtern mit Kotlin](https://medium.com/@kevalpatel2106/generating-random-password-in-kotlin-99dccfdaf-9b15d0771bfc)
- [Einsatz der Random-Klasse in Spielen](https://gamedevelopment.tutsplus.com/tutorials/randomness-in-games--gamedev-344)