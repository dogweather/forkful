---
title:                "Swift: Ausgabe von Debug-Informationen drucken"
simple_title:         "Ausgabe von Debug-Informationen drucken"
programming_language: "Swift"
category:             "Swift"
tag:                  "Testing and Debugging"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/de/swift/printing-debug-output.md"
---

{{< edit_this_page >}}

## Warum

Beim Entwickeln von Apps kann es manchmal schwierig sein, zu verstehen, was im Code genau passiert. Hierbei kann das Ausgeben von Debug-Meldungen sehr hilfreich sein. Durch das Anzeigen von bestimmten Werten oder Variablen können wir besser verstehen, wie der Code ausgeführt wird und mögliche Fehler schneller identifizieren.

## So geht's

Um Debug-Ausgaben in Swift zu erstellen, können wir die Funktion `print()` verwenden. Diese Funktion gibt den Inhalt innerhalb der Klammern in der Konsole aus.

Hier ist ein einfaches Beispiel:

```Swift
let name = "Max"
print("Mein Name ist \(name)")
```
Dieses Beispiel würde "Mein Name ist Max" in der Konsole ausgeben.

Wir können auch Variablen in der `print()` Funktion verwenden, um den Wert einer Variable anzuzeigen.

```Swift
let num1 = 10
let num2 = 5
print("\(num1) + \(num2) = \(num1 + num2)")
```
Dieses Beispiel würde "10 + 5 = 15" in der Konsole ausgeben.

## Tiefergehende Informationen

Neben einfachen Texten und Variablen können wir auch komplexe Datenstrukturen wie Arrays, Dictionaries und Objekte in der `print()` Funktion ausgeben.

Wir können auch Formatierungszeichen verwenden, um die Ausgabe übersichtlicher zu gestalten. Zum Beispiel können wir `\n` verwenden, um einen Zeilenumbruch zu erzeugen oder `\t` für einen Tabulator.

```Swift
let prices = [20.5, 30.7, 10.2]
let total = prices.reduce(0, +)
print("Die Gesamtsumme der Preise beträgt: \n \(total)")
```

Dieses Beispiel würde "Die Gesamtsumme der Preise beträgt: 61.4" ausgeben.

Außerdem können wir die Optionen `terminator` und `separator` verwenden, um anzugeben, was zwischen den einzelnen Ausgaben erscheinen soll.

```Swift
let fruits = ["Apfel", "Banane", "Orange"]
print("Meine Lieblingsfrüchte sind:", terminator: " ")
for fruit in fruits {
    print(fruit, terminator: ", ")
}
```

Dieses Beispiel würde "Meine Lieblingsfrüchte sind: Apfel, Banane, Orange" ausgeben.

## Siehe auch

- [Apple Dokumentation zu print()](https://developer.apple.com/documentation/swift/2893203-print)
- [Video Tutorial: Debugging in Swift](https://www.youtube.com/watch?v=VZXXN5vbD4g)
- [5 Tipps für effektives Debugging](https://medium.com/@ayanonagon/5-practical-tips-for-debugging-in-swift-bf9bd539b2ba)