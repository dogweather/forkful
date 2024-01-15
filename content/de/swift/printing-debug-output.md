---
title:                "Ausgabe von Debug-Informationen"
html_title:           "Swift: Ausgabe von Debug-Informationen"
simple_title:         "Ausgabe von Debug-Informationen"
programming_language: "Swift"
category:             "Swift"
tag:                  "Testing and Debugging"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/de/swift/printing-debug-output.md"
---

{{< edit_this_page >}}

## Warum

Debugausgabe ist ein entscheidendes Werkzeug bei der Entwicklung von Software. Mit Hilfe von Debugausgaben können wir den Fluss des Programms verfolgen, Variablenwerte überprüfen und eventuelle Fehler identifizieren. Dadurch wird die Fehlersuche und Fehlerbehebung wesentlich einfacher.

## So geht's

Um Debugausgaben in Swift zu verwenden, können wir die `print()` Funktion verwenden. Hier ist ein Beispiel:

```Swift
let name = "Max"
let age = 25

print("Name:", name, "Alter:", age)
```

Die Ausgabe dieses Codes sieht wie folgt aus:

```
Name: Max Alter: 25
```

Wie Sie sehen können, können wir mehrere Argumente an die `print()` Funktion übergeben und sie werden automatisch durch Leerzeichen getrennt.

Wir können auch Variablenwerte direkt in der Debugausgabe überprüfen, indem wir sie in geschweifte Klammern setzen:

```Swift
print("Das Alter von \(name) ist \(age)")
```

Die Ausgabe dieses Codes wäre:

```
Das Alter von Max ist 25
```

Wir können sogar bedingte Ausgaben erstellen, indem wir die `print()` Funktion innerhalb von `if` -Bedingungen verwenden:

```Swift
if name == "Max" {
    print("Willkommen, Max!")
} else {
    print("Willkommen, Fremder!")
}
```

Die Ausgabe dieses Codes wäre:

```
Willkommen, Max!
```

## Tiefergehende Informationen

Es gibt viele weitere Funktionen, die wir nutzen können, um unsere Debugausgaben an unsere Bedürfnisse anzupassen. Zum Beispiel können wir die Trennzeichen und das Endzeichen der Debugausgabe ändern, sowie viele weitere Optionen.

Um mehr über die `print()` Funktion und ihre Möglichkeiten zu erfahren, können Sie die offizielle Dokumentation von Apple lesen: https://developer.apple.com/documentation/swift/1541053-print.

## Siehe auch

- [Offizielle Swift Webseite](https://swift.org/)
- [Swift für Anfänger: Ein Leitfaden zur Programmierung in Swift](https://thenextweb.com/news/swift-for-beginners-a-programming-guide)