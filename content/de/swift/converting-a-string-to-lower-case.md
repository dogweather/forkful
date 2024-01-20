---
title:                "Einen String in Kleinbuchstaben umwandeln"
html_title:           "Elm: Einen String in Kleinbuchstaben umwandeln"
simple_title:         "Einen String in Kleinbuchstaben umwandeln"
programming_language: "Swift"
category:             "Swift"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/de/swift/converting-a-string-to-lower-case.md"
---

{{< edit_this_page >}}

## Was & Warum?

Die Konvertierung eines Strings in Kleinbuchstaben ist eine einfache Operation, die in der Programmierung erheblich zur Datenkonsistenz beiträgt. Programmierer nutzen dies, um die Nutzereingabe zu harmonisieren, so dass "Hallo" und "hallo" effektiv gleich sind.

## So geht's:

In Swift kann man einen String ganz einfach in Kleinbuchstaben umwandeln - mit der Methode `lowercased()`:

```Swift
let originalString = "Hallo Welt!"
let lowercasedString = originalString.lowercased()
print(lowercasedString)   // Ausgabe: "hallo welt!"
```

## Vertiefung

Die `lowercased()` Methode in Swift wurde auf der Grundlage des Unicode-Standards implementiert. Historisch gesehen gibt es verschiedene Möglichkeiten, Strings in Kleinbuchstaben umzuwandeln, abhängig von der verwendeten Sprache und dem gewählten Zeichensatz.

Alternativ könnten Sie auch eine Schleife verwenden, um jeden Buchstaben einzeln in einen Kleinbuchstaben zu konvertieren, aber das ist umständlicher und weniger effizient.

Bei der Umwandlung von Strings in Kleinbuchstaben ist zu beachten, dass dies in einigen Kulturen und Sprachen, wie der türkischen, nicht unkompliziert ist, da sie spezielle Regeln für die Groß- und Kleinschreibung haben. Swift behandelt dies durch die automatische Anwendung der lokal spezifischen Konvertierungslogik.

## Mehr dazu

- Die offizielle Swift Dokumentation zur `lowercased()` Methode: [Swift.org - lowercased()](https://developer.apple.com/documentation/swift/string/2997127-lowercased)
- Informationen über Unicode und Zeichenumwandlungen: [Unicode.org - Case Operations](https://unicode.org/notes/tn21/)