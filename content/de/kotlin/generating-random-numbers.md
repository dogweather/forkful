---
title:                "Zufallszahlen generieren"
html_title:           "Kotlin: Zufallszahlen generieren"
simple_title:         "Zufallszahlen generieren"
programming_language: "Kotlin"
category:             "Kotlin"
tag:                  "Numbers"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/de/kotlin/generating-random-numbers.md"
---

{{< edit_this_page >}}

## Was & Warum?

Generieren von Zufallszahlen ist ein wichtiger Bestandteil der Programmierung, bei dem Zahlen von einem Computer zufällig ausgewählt und ausgegeben werden. Programmierer verwenden dies, um verschiedene Szenarien zu simulieren und mögliche Ausgänge zu testen.

## Wie geht's?

Kotlin bietet eine integrierte Funktion namens `random()`, die es uns ermöglicht, eine Zufallszahl innerhalb eines bestimmten Bereichs zu generieren. Wir müssen nur den Bereich angeben, aus dem die Zahl ausgewählt werden soll, und dann `random()` aufrufen. Zum Beispiel:

```Kotlin
fun main() {
    val randomNumber = (1..10).random()
    println("Die generierte Zufallszahl ist: $randomNumber")
}
```
Die Ausgabe könnte beispielsweise "Die generierte Zufallszahl ist: 6" sein. Wie Sie sehen, wird mit Hilfe der `random()` Funktion eine Zufallszahl aus dem Bereich von 1 bis 10 ausgewählt und ausgegeben.

## Tiefere Einblicke

Die Verwendung von Zufallszahlen in der Programmierung hat eine lange Geschichte, die bis in die Anfänge der Computertechnologie zurückreicht. Zu den alternativen Methoden für die Generierung von Zufallszahlen gehören das Verwenden von physikalischen Eigenschaften von Computern wie Stromfluss und andere komplexe mathematische Algorithmen.

In Kotlin ist die `random()` Funktion eine pseudo-zufällige Generierungsmethode, was bedeutet, dass die Zahlen vorhersehbar sind, aber dennoch zufällig genug für die meisten Anwendungsfälle sind. Um wirklich zufällige Zahlen zu generieren, wären spezielle Geräte oder Dienste erforderlich.

## Siehe auch

Weitere Informationen zur Verwendung der `random()` Funktion finden Sie in der offiziellen [Kotlin-Dokumentation](https://kotlinlang.org/api/latest/jvm/stdlib/kotlin.random/random.html). Wenn Sie tiefer in die Thematik einsteigen möchten, empfehlen wir die Lektüre dieses [Artikels](https://medium.com/@sullyfchen/how-is-kotlin-s-random-fixed-memory-related-to-security-79d0f244f972) über die Sicherheit der `random()` Funktion in Kotlin.