---
title:    "Kotlin: Zufallszahlen erzeugen"
keywords: ["Kotlin"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/de/kotlin/generating-random-numbers.md"
---

{{< edit_this_page >}}

## Warum

Das Generieren von zufälligen Zahlen ist ein wichtiger Aspekt in der Programmierung. Es ermöglicht die Erstellung von dynamischen und abwechslungsreichen Anwendungen und stellt sicher, dass jedes Mal, wenn das Programm ausgeführt wird, unterschiedliche Ergebnisse erzielt werden. In diesem Blog-Beitrag werden wir uns ansehen, wie wir in Kotlin zufällige Zahlen generieren können.

## Wie geht man vor

Um in Kotlin zufällige Zahlen zu generieren, können wir die `Random` Klasse verwenden. Sie bietet verschiedene Methoden, um unterschiedliche Arten von zufälligen Werten zu erzeugen. Hier sind einige Beispiele:

```Kotlin
val random = Random()

// Zufällige Ganzzahl zwischen 0 und 100
val randomNumber = random.nextInt(101)

// Zufälliger double-Wert zwischen 0.0 (inklusive) und 1.0 (exklusive)
val randomDouble = random.nextDouble()

// Zufälliger boolean-Wert
val randomBoolean = random.nextBoolean()
```

Die `Random` Klasse verwendet die Systemzeit als "Seed", um die Zufälligkeit zu erzeugen. Wenn du jedoch den Seed selbst festlegen möchtest, kannst du auch den entsprechenden Konstruktor verwenden. Zum Beispiel:

```Kotlin
val random = Random(1234) // Seed = 1234
```

Um mehrere zufällige Zahlen zu generieren, kannst du eine Schleife verwenden und die `nextInt()` oder `nextDouble()` Methode in jeder Iteration aufrufen. Hier ist ein Beispiel mit einer einfachen `for`-Schleife:

```Kotlin
for (i in 1..10) {
    val randomNum = random.nextInt(101)
    println(randomNum) // Zum Bildschirm ausgeben oder in eine Liste speichern
}
```

Ein weiterer interessanter Aspekt beim Generieren von zufälligen Zahlen ist, dass du die Verteilung des generierten Werts beeinflussen kannst. Dazu kannst du die `nextGaussian()` Methode verwenden, die eine normalverteilte Zufallszahl mit einer Mittelwert von 0 und einer Standardabweichung von 1 zurückgibt. Hier ist ein Beispiel:

```Kotlin
val randomNumber = random.nextGaussian() * 10 // Mittelwert von 0 und Standardabweichung von 10
```

Es gibt viele weitere Methoden in der `Random` Klasse, um zufällige Zahlen zu generieren. Du kannst die offizielle Dokumentation überprüfen, um mehr darüber zu erfahren.

## Tiefer schürfen

Beim Generieren von zufälligen Zahlen ist es wichtig zu verstehen, dass diese Werte nicht wirklich "zufällig" sind, sondern durch einen Algorithmus berechnet werden. Dies bedeutet, dass es möglich ist, dass dieselben Werte wiederholt auftreten. Es ist auch wichtig zu beachten, dass die Qualität der Zufälligkeit von der verwendeten Methode abhängt.

In Kotlin empfiehlt es sich, die `Random` Klasse zu verwenden, da sie optimiert wurde und in der Regel ausreichend gute Zufälligkeit bietet. Wenn du jedoch aus Gründen der Sicherheit oder für wissenschaftliche Zwecke höhere Anforderungen an die Zufälligkeit hast, gibt es auch spezielle Bibliotheken, die sich auf das Generieren von Krypto-zufälligen Zahlen spezialisiert haben.

## Siehe auch

- [Offizielle Kotlin Dokumentation zur Random Klasse](https://kotlinlang.org/api/latest/jvm/stdlib/kotlin.random/-random/)
- [Stackoverflow-Beitrag zum Generieren von zufälligen Zahlen in Kotlin](https://stackoverflow.com/questions/45685074/creating-a-random-number-with-kotlin)
- [Blog-Beitrag über Krypto-zufällige Zahlen in Kotlin](https://medium.com/@sidharth_reddy/generating-cryptographically-secured-random-values-in-android-kotlin-bb161bc3f8de)