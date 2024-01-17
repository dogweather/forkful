---
title:                "Erzeugung von Zufallszahlen"
html_title:           "Swift: Erzeugung von Zufallszahlen"
simple_title:         "Erzeugung von Zufallszahlen"
programming_language: "Swift"
category:             "Swift"
tag:                  "Numbers"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/de/swift/generating-random-numbers.md"
---

{{< edit_this_page >}}

## Was & Warum?
Generieren zufälliger Zahlen bedeutet, dass Programmierer Zahlen in einem bestimmten Bereich erzeugen, ohne dabei ein vorhersehbares Muster zu folgen. Dies kann nützlich sein für Simulationen, Spiele oder andere Anwendungen, die auf Zufallswerten basieren.

## So geht's:
Um zufällige Zahlen in Swift zu generieren, verwenden wir die Funktion `arc4random_uniform()`. Diese Funktion erwartet als Argument eine positive Ganzzahl, die den oberen Bereich der generierten Zahlen angibt. Das Ergebnis ist eine zufällige Zahl im Bereich von 0 bis zur angegebenen Zahl, ausschließlich der angegebenen Zahl selbst. 
```Swift
let number = arc4random_uniform(100)
print(number) // Beispiel-Ausgabe: 47
```
Um Zahlen in einem anderen Bereich zu generieren, können wir einfach den Bereich anpassen.
```Swift
let number = arc4random_uniform(25) + 10 // Zahlen zwischen 10 und 34
print(number) // Beispiel-Ausgabe: 23
```

## Deep Dive:
Die Funktion `arc4random_uniform()` basiert auf dem C-Befehl `arc4random()` und wurde von Apple für die Verwendung in Swift angepasst. Sie wurde entwickelt, um die Verteilung von Zufallszahlen auszugleichen und eine bessere Verteilung als andere Methoden zu erzielen. Alternativen zur Generierung von Zufallszahlen in Swift sind die Funktionen `Int.random()` und `Double.random()`, die mehr Flexibilität bei der Angabe des Zahlenbereichs bieten, aber keine so ausgewogene Verteilung bieten wie `arc4random_uniform()`. Bei der Implementierung der Funktion `arc4random_uniform()` wird ein Pseudozufallszahlengenerator verwendet, der auf dem Linear Congruential Generator (LCG) basiert und bestimmte Parameter verwendet, um Zahlen zu generieren.

## Siehe auch:
Weitere Informationen zur Verwendung von Zufallszahlen in Swift finden Sie in der offiziellen Dokumentation von Apple: https://developer.apple.com/documentation/swift/int/1967613-random. Eine gute Ressource für die Tiefe des Themas ist das Buch "Random Number Generation and Monte Carlo Methods" von James E. Gentle.