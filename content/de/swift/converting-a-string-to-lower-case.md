---
title:                "Umwandlung einer Zeichenkette in Kleinbuchstaben"
html_title:           "Swift: Umwandlung einer Zeichenkette in Kleinbuchstaben"
simple_title:         "Umwandlung einer Zeichenkette in Kleinbuchstaben"
programming_language: "Swift"
category:             "Swift"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/de/swift/converting-a-string-to-lower-case.md"
---

{{< edit_this_page >}}

## Was & Warum?
Beim Umwandeln eines Strings in Kleinbuchstaben handelt es sich um eine Methode in der Programmierung, bei der alle Großbuchstaben in einem String zu Kleinbuchstaben umgewandelt werden. Programmierer nutzen dies, um sicherzustellen, dass die Eingabe unabhängig von der Groß- und Kleinschreibung korrekt verarbeitet wird.

## Wie geht's?
Das Konvertieren eines Strings in Kleinbuchstaben in Swift ist einfach. Verwenden Sie die Methode "lowercased()" auf dem betreffenden String und weisen Sie das Ergebnis einem neuen String zu. Hier ist ein Beispiel:

```Swift
let string = "HALLO WELT"
let lowercaseString = string.lowercased()
print(lowercaseString) // ausgabe: hallo welt
```

## Tief Eintauchen
Das Konzept, Großbuchstaben in Kleinbuchstaben umzuwandeln, existiert seit Beginn der Programmierung und wird in vielen Sprachen unterstützt. In Swift gibt es auch eine alternative Methode "lowercased(with:)", die es ermöglicht, anzugeben, in welchem Sprachbereich die Umwandlung durchgeführt werden soll. Die Implementierung von "lowercased()" basiert auf Unicode-Regeln und ist daher sehr zuverlässig.

## Siehe auch
Weitere Informationen zum Thema Zeichenketten und Unicode in Swift finden Sie in der offiziellen Swift-Dokumentation: https://developer.apple.com/documentation/swift/string