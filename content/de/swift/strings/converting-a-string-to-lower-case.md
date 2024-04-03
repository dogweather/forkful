---
date: 2024-01-20 17:39:08.481933-07:00
description: 'So geht''s: Swift macht die Konvertierung von Zeichenketten in Kleinbuchstaben
  einfach mit der `lowercased()` Methode.'
lastmod: '2024-03-13T22:44:54.213119-06:00'
model: gpt-4-1106-preview
summary: Swift macht die Konvertierung von Zeichenketten in Kleinbuchstaben einfach
  mit der `lowercased()` Methode.
title: Umformung eines Strings in Kleinbuchstaben
weight: 4
---

## So geht's:
Swift macht die Konvertierung von Zeichenketten in Kleinbuchstaben einfach mit der `lowercased()` Methode.

```Swift
let meinText = "Das IST ein Test!"
let textInKleinbuchstaben = meinText.lowercased()
print(textInKleinbuchstaben)
// Ausgabe: "das ist ein test!"
```

Der Code demonstriert die Umwandlung einer Zeichenkette in Kleinbuchstaben. `lowercased()` ist eine Methode, die aufgerufen wird, um den Text zu konvertieren.

## Deep Dive
Die Methode `lowercased()` wurde in Swift integriert, um Entwicklern eine einfache Möglichkeit zu bieten, String-Manipulationen durchzuführen. In anderen Programmiersprachen wie Python oder JavaScript gibt es ähnliche Funktionen. 

Alternativ kannst Du die Zeichen einer Zeichenkette einzeln durchgehen und manuell in Kleinbuchstaben umwandeln, was jedoch umständlich ist und Fehlerquellen enthält. Die Standardbibliotheksfunktion `lowercased()` berücksichtigt außerdem Lokalisierung und Sprachspezifika, was bei manueller Implementierung meist übersehen wird.

Die Implementierung in Swift nutzt Unicode-Standard, um sicherzustellen, dass auch internationale Schriftsysteme korrekt verarbeitet werden.

## Siehe Auch
- Swift Standard Library: `String` https://developer.apple.com/documentation/swift/string
- Unicode Standard: https://unicode.org/standard/standard.html
- W3Schools String Methods Reference: https://www.w3schools.com/swift/swift_string_methods.asp
