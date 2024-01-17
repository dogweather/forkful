---
title:                "Untersuchen von Teilzeichenketten"
html_title:           "Swift: Untersuchen von Teilzeichenketten"
simple_title:         "Untersuchen von Teilzeichenketten"
programming_language: "Swift"
category:             "Swift"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/de/swift/extracting-substrings.md"
---

{{< edit_this_page >}}

## Was & Warum?
Das Extrahieren von Teilzeichenketten bezieht sich auf das Aufteilen einer längeren Zeichenkette in kleinere Abschnitte. Programmierer machen das, um bestimmte Informationen aus einer Zeichenkette zu gewinnen oder um sie in einem anderen Format zu präsentieren.

## Wie funktioniert das?
Um eine Teilzeichenkette in Swift zu extrahieren, können wir die Methode `substring` nutzen. Diese erwartet zwei Parameter: den Startindex und die Länge der gewünschten Teilzeichenkette. Denken wir zum Beispiel an den String "Hallo Welt", dann können wir den Text "Hallo" extrahieren, indem wir den Startindex als 0 und die Länge als 5 angeben.

```Swift
let text = "Hallo Welt"
let substring = text.substring(from: 0, length: 5)
print(substring) // Ausgabe: Hallo
```

Wir können auch den zweiten Parameter weglassen, um den Rest der Zeichenkette ab dem angegebenen Startindex zu extrahieren.

```Swift
let text = "Hallo Welt"
let substring = text.substring(from: 6)
print(substring) // Ausgabe: Welt
```

## Tiefgründige Informationen
Das Extrahieren von Teilzeichenketten gibt es schon seit den Anfängen der Programmierung. Es ist eine nützliche Methode, um Texte zu manipulieren und zu formatieren. Alternativen zu `substring` sind beispielsweise die Methoden `prefix` und `suffix`, die ähnliche Funktionen haben. Bei der Implementierung von `substring` muss beachtet werden, dass der Startindex nicht größer sein darf als die Länge der Zeichenkette, sonst kommt es zu einem Fehler.

## Weitere Quellen
- [Apple Dokumentation zu `substring`](https://developer.apple.com/documentation/swift/string/2817616-substring)
- [Beispiele zum Extrahieren von Teilzeichenketten in Swift](https://www.hackingwithswift.com/example-code/strings/how-to-extract-a-substring-from-a-string)
- [Weitere nützliche String-Methoden in Swift](https://www.swiftbysundell.com/articles/working-with-strings-in-swift/)