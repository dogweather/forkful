---
title:    "Swift: Zufallszahlen generieren"
keywords: ["Swift"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/de/swift/generating-random-numbers.md"
---

{{< edit_this_page >}}

## Warum

Die Generierung von Zufallszahlen ist ein wichtiges Konzept in der Programmierung, da sie es uns ermöglicht, verschiedene Arten von Daten oder Aktionen zufällig auszuwählen. Dies kann beispielsweise für die Erstellung von Spielen, Simulationen oder zur Verbesserung von Sicherheitsmaßnahmen verwendet werden.

## So geht's

Um in Swift zufällige Zahlen zu generieren, können wir die `arc4random_uniform()` Funktion verwenden. Diese Funktion gibt eine zufällige Ganzzahl zurück, die zwischen 0 und dem als Parameter übergebenen Maximum liegt. Hier ist ein Beispielcode, der zufällig eine Zahl zwischen 1 und 10 auswählt und sie ausgibt:

```Swift
let randomNumber = Int(arc4random_uniform(10)) + 1
print(randomNumber)
```

Die Verwendung von `arc4random_uniform()` ist nicht auf Ganzzahlen beschränkt. Wir können auch zufällige Gleitkommazahlen mit der `drand48()` Funktion generieren. Diese Funktion gibt eine Zahl zwischen 0 und 1 zurück. Um eine zufällige Gleitkommazahl zwischen zwei festgelegten Werten zu generieren, müssen wir einfach die Rückgabewert der Funktion mit dem gewünschten Bereich multiplizieren. Hier ist ein Beispiel:

```Swift
let randomDecimalNumber = drand48() * 10.0 //generates a random number between 0 and 10
print(randomDecimalNumber)
```

Wir können auch ein Array mit zufälligen Elementen erstellen, indem wir die `shuffle()` Methode verwenden. Diese Methode mischt die Elemente des Arrays in zufälliger Reihenfolge. Hier ist ein Beispielcode:

```Swift
var array = [1, 2, 3, 4, 5]
array.shuffle() //shuffles the elements randomly
print(array)
```

## Tiefer Einblick

Bei der Generierung von Zufallszahlen ist es wichtig zu verstehen, dass diese Zahlen nicht wirklich "zufällig" sind, sondern auf einem deterministischen Algorithmus basieren. Daher sind sie nicht für Sicherheitszwecke geeignet. Um jedoch sicherzustellen, dass die generierten Zahlen so zufällig wie möglich sind, sollte die Funktion, die wir verwenden, eine gleichmäßige Verteilung aufweisen. Dies bedeutet, dass jede mögliche Zahl mit der gleichen Wahrscheinlichkeit vorkommen sollte.

Wenn wir in bestimmten Fällen eine andere Verteilung wünschen, können wir auch benutzerdefinierte Funktionen erstellen, die eine bestimmte Verteilung von Zufallszahlen generieren. Dies ist jedoch ein fortgeschrittenes Thema und geht über den Rahmen dieses Blog-Beitrags hinaus.

## Siehe auch

- [Apple Swift Dokumentation über Zufallszahlen](https://developer.apple.com/documentation/swift/swift_standard_library/classes/arc4random_uniform)
- [Beispiele für die Verwendung von Zufallszahlen in Spielen mit Swift](https://medium.com/@infiniteooo/zufallszahlen-in-swift-mit-arc4random-f9cf826a7f47)
- [Fortgeschrittenere Konzepte zur Generierung von Zufallszahlen in Swift](https://medium.com/swift-programming/swift-playground-tutorial-random-numbers-in-swift-2-6f10433a6eac)