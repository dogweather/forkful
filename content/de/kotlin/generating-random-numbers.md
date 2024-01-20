---
title:                "Zufallszahlen generieren"
html_title:           "Arduino: Zufallszahlen generieren"
simple_title:         "Zufallszahlen generieren"
programming_language: "Kotlin"
category:             "Kotlin"
tag:                  "Numbers"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/de/kotlin/generating-random-numbers.md"
---

{{< edit_this_page >}}

## Was und Warum?

Das Generieren von Zufallszahlen ist eine gängige Programmierungsaufgabe, die eingesetzt wird, um Unvorhersehbarkeit in Software zu schaffen. Programmierer tun dies oft für Spiele, Simulationen oder Sicherheitsalgorithmen, um nur einige Beispiele zu nennen.

## So Geht's:

In Kotlin gibt es verschiedene Möglichkeiten, um Zufallszahlen zu generieren. Hier sind einige Beispiele:

```Kotlin
// Generiert eine Zufallszahl von 0 (einschließlich) bis 100 (ausschließlich)
val randomInt = (0 until 100).random()
println("Zufalls Integer: $randomInt")

// Generiert eine Zufalls-Gleitkommazahl von 0.0 (einschließlich) bis 1.0 (ausschließlich)
val randomDouble = Math.random()
println("Zufalls Double: $randomDouble")
```

## Tiefgang

Zufallszahlen waren schon immer ein wichtiger Bestandteil der Informatik. Die ersten Computer nutzten physikalische Prozesse, wie radioaktiven Zerfall oder Rauschen, um echte Zufallszahlen zu generieren. Heute nutzen die meisten Maschinen statistische Modelle oder deterministische Algorithmen, um Pseudozufallszahlen zu generieren.

Alternativ zu den oben genannten Methoden kann auch die Funktion `Random.nextInt()` der Bibliothek `java.util` verwendet werden.

Die Implementierung von Zufallszahlengeneratoren in Kotlin basiert auf den Java-Standards, ist aber einfacher gestaltet und leichter zu verstehen.

## Siehe Auch

Weitere Informationen zum Thema finden Sie unter folgenden Links:

- [Kotlin’s Random API](https://kotlinlang.org/api/latest/jvm/stdlib/kotlin.random/-random)
- [Java’s Random Class](https://docs.oracle.com/javase/8/docs/api/java/util/Random.html)