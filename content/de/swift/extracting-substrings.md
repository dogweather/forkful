---
title:                "Teilzeichenketten extrahieren"
html_title:           "PowerShell: Teilzeichenketten extrahieren"
simple_title:         "Teilzeichenketten extrahieren"
programming_language: "Swift"
category:             "Swift"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/de/swift/extracting-substrings.md"
---

{{< edit_this_page >}}

# Extrahieren von Teilstrings in Swift: Ein nützliches Werkzeug für Programmierer

## Was & Warum?
Das Extrahieren von Teilstrings in Swift ist ein Prozess, bei dem wir spezifische Teile eines Textes herausschneiden. Programmierer tun dies, um den Text zu analysieren, Daten zu manipulieren oder den Text auf verschiedene Weisen zu prüfen. 

## Wie man:
Hier sind einige Beispiele und deren Ausgabe:

```Swift
let text = "Hallo, ich bin ein Swift-Programmierer"
let indexStart = text.index(text.startIndex, offsetBy: 7)
let indexEnd = text.index(text.startIndex, offsetBy: 14)
let teil = text[indexStart...indexEnd]
print(teil) // Ausgabe: "ich bin"
```

In diesem Beispiel haben wir den Teilstring "ich bin" aus dem gegebenen Text extrahiert. 

```Swift
let name = "Max Mustermann"
let firstSpace = name.firstIndex(of: " ") ?? name.endIndex
let firstName = name[..<firstSpace]
print(firstName) // Ausgabe: "Max"
```
In diesem Fall haben wir den Vornamen "Max" extrahiert, indem wir bis zum ersten Leerzeichen gesucht haben.

## Vertiefung:
(1) Historischer Kontext: Vor Swift 4.0 war das Extrahieren von Teilstrings in Swift ziemlich umständlich und kompliziert. Swift 4.0 hat eine neue ``Substring`` Struktur eingeführt, um den Prozess zu vereinfachen. 

(2) Alternativen: Es gibt viele Möglichkeiten, Teilstrings in Swift zu extrahieren. Eine andere Alternative ist die Verwendung von Range:

```Swift
let range = text.range(of: "ich bin")
let teil2 = text[range!] 
print(teil2) // Ausgabe: "ich bin"
```

(3) Implementierungsdetails: In Swift sind `String` und `Substring` eng verbunden. Ein `Substring` teilt sich den Speicher mit dem Originalstring und hält nur einen Zeiger auf den Anfang, das Ende und den Text.

## Siehe auch:
1. Apple's officielle Dokumentation: ["Substring"](https://developer.apple.com/documentation/swift/substring)
2. [Swift-String und deren Manipulation](https://www.hackingwithswift.com/articles/115/introduction-to-string-manipulation-in-swift)
3. [Arbeiten mit Strings in Swift](https://www.swiftbysundell.com/articles/strings-in-swift/)