---
title:                "Debug-Ausgabe drucken"
html_title:           "Kotlin: Debug-Ausgabe drucken"
simple_title:         "Debug-Ausgabe drucken"
programming_language: "Kotlin"
category:             "Kotlin"
tag:                  "Testing and Debugging"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/de/kotlin/printing-debug-output.md"
---

{{< edit_this_page >}}

## Warum

Debugging ist eine wichtige Fähigkeit für jeden Programmierer, um Fehler in einem Code zu finden und zu beheben. Die Ausgabe von Debug-Informationen ist ein nützliches Werkzeug, um den Ablauf des Codes zu verfolgen und potenzielle Probleme zu erkennen. In diesem Artikel werden wir uns ansehen, wie man Debug-Informationen in Kotlin ausgeben kann.

## Wie geht's

Um in Kotlin Debug-Informationen auszugeben, können wir die Funktion `println()` verwenden. Diese Funktion gibt den Inhalt der Klammern in der Konsole aus. Schauen wir uns ein Beispiel an:

```Kotlin
fun main() {
    val name = "Max"
    println("Hallo $name, willkommen zu unserem Programm!")
}
```

Die Ausgabe dieses Codes wird sein:

```
Hallo Max, willkommen zu unserem Programm!
```

Wie wir sehen können, verwenden wir die Interpolations-Syntax von Kotlin, um den Wert der Variable `name` in den String einzufügen.

Wir können auch die `println()` Funktion verwenden, um den Wert von Variablen oder Ausdrücken auszugeben, die während der Laufzeit berechnet werden. Schauen wir uns ein Beispiel an:

```Kotlin
fun main() {
    val x = 5
    val y = 10
    println("Die Summe von $x und $y ist ${x + y}")
}
```

Die Ausgabe dieses Codes wird sein:

```
Die Summe von 5 und 10 ist 15
```

Somit können wir `println()` verwenden, um unseren Code während der Laufzeit zu überwachen und mögliche Fehler zu erkennen.

## Tiefere Einblicke

Neben der `println()` Funktion gibt es noch weitere Möglichkeiten, Debug-Informationen in Kotlin auszugeben. Eine davon ist die Verwendung des `Logger` Objects aus der Java Standard Library. Dieses Object bietet verschiedene Methoden zum Ausgeben von Debug-Informationen, die je nach Schweregrad der Nachricht ausgewählt werden können.

Ein weiteres nützliches Werkzeug ist die Verwendung der `Log` Klasse aus der Android SDK. Diese Klasse bietet auch eine Reihe von Methoden zum Ausgeben von Debug-Informationen, die speziell für die Entwicklung von Android-Apps optimiert sind.

Es ist auch möglich, Debug-Informationen in einer externen Datei auszugeben, anstatt sie in der Konsole anzuzeigen. Hierfür können wir die `FileWriter` Klasse aus der Java Standard Library verwenden. Diese ermöglicht es uns, eine Datei zu erstellen und Inhalte in sie zu schreiben, einschließlich Debug-Informationen.

## Siehe auch

- [Offizielle Dokumentation zu Debugging in Kotlin](https://kotlinlang.org/docs/reference/debugging.html)
- [Tutorial: Debugging in Kotlin](https://www.baeldung.com/kotlin/debugging)
- [Medium-Artikel: Debugging in Kotlin](https://medium.com/@avigezerit/debugging-in-kotlin-777e7f082d0a)