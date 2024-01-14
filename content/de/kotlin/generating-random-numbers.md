---
title:                "Kotlin: Eineinführung in die Erzeugung von Zufallszahlen"
simple_title:         "Eineinführung in die Erzeugung von Zufallszahlen"
programming_language: "Kotlin"
category:             "Kotlin"
tag:                  "Numbers"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/de/kotlin/generating-random-numbers.md"
---

{{< edit_this_page >}}

## Warum

Die Generierung von Zufallszahlen ist ein wichtiger Bestandteil der Programmierung. Es kann verwendet werden, um Spiele zu erstellen, zufällige Auswahlentscheidungen zu treffen oder Daten zu verschleiern. Mit Kotlin können Sie ganz einfach Zufallszahlen erzeugen und in Ihre Programme integrieren.

## Wie geht es

Um Zufallszahlen in Kotlin zu generieren, müssen Sie die Klasse "Random" verwenden. Zunächst müssen Sie ein Objekt von dieser Klasse erstellen, indem Sie den Konstruktor aufrufen. Dann können Sie die verschiedenen Methoden nutzen, um Zufallszahlen in verschiedenen Formaten zu erhalten. Zum Beispiel:

```Kotlin
// Erstellen eines Objekts der Klasse Random
val random = Random()

// Generieren einer Zufallszahl zwischen 0 und 10
val number = random.nextInt(11)

// Generieren einer Zufallszahl zwischen 0 und 1
val decimal = random.nextDouble()
```

Die verwendeten Methoden variieren je nachdem, was für Zufallszahlen Sie benötigen. Sie können auch bestimmte Grenzen und Bedingungen für die Zufallszahlen festlegen, indem Sie zusätzliche Parameter in die Methoden aufnehmen. Für weitere Beispiele und Informationen zur Generierung von Zufallszahlen in Kotlin können Sie die offizielle Dokumentation konsultieren.

## Tiefer Einblick

Hinter den Kulissen verwendet Kotlin die Klasse "java.util.Random", um Zufallszahlen zu generieren. Diese Klasse stellt verschiedene Methoden zur Verfügung, um Zufallszahlen in unterschiedlichen Formaten und mit verschiedenen Bedingungen zu erzeugen. Kotlin macht es jedoch einfacher, darauf zuzugreifen und zu verwenden, indem es eine benutzerfreundliche Syntax bietet.

Es ist auch wichtig zu beachten, dass Zufallszahlen, die auf diese Weise generiert werden, nicht wirklich zufällig sind. Sie basieren auf einem sogenannten "Seed" oder Startwert, der vom Programm festgelegt wird. Wenn also dasselbe Programm mit demselben Seed ausgeführt wird, werden dieselben Zufallszahlen generiert. Dies kann in manchen Fällen unerwünscht sein, daher gibt es Techniken, um den Seed zu variieren und sicherzustellen, dass die Zufallszahlen jedes Mal anders sind.

## Siehe auch

- [Offizielle Dokumentation zu Kotlin Random](https://kotlinlang.org/api/latest/jvm/stdlib/kotlin.random/-random/)
- [Tutorial zu Zufallszahlen in Kotlin](https://www.baeldung.com/kotlin/random)