---
title:    "Swift: Die Länge eines Strings finden."
keywords: ["Swift"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/de/swift/finding-the-length-of-a-string.md"
---

{{< edit_this_page >}}

# Warum

Die Länge eines Strings zu finden, ist eine grundlegende Aufgabe beim Programmieren mit Swift. Es ist wichtig, diese Funktionalität zu verstehen, da sie in vielen Anwendungen verwendet wird, um Text zu analysieren und zu manipulieren.

# Wie man es macht

Es gibt verschiedene Möglichkeiten, die Länge eines Strings in Swift zu finden. Hier ist ein Beispiel, wie es mit der `count` Methode geht:

```Swift
let text = "Hallo Welt!"

print(text.count)

// Ausgabe: 11
```

Hier wird die `count` Methode auf den String `text` angewendet, um die Anzahl der Zeichen in dem String zu erhalten. Diese Methode gibt eine ganze Zahl zurück, die die Länge des Strings repräsentiert.

# Tieferer Einblick

Eine andere Methode, um die Länge eines Strings zu finden, ist die Verwendung der `characters` Eigenschaft. Diese gibt eine Sammlung von Zeichen im String zurück, die dann gezählt werden können.

```Swift
let text = "Apple"

print(text.characters.count)

// Ausgabe: 5
```

Es ist auch wichtig zu beachten, dass die Länge eines Strings nicht unbedingt der Anzahl der sichtbaren Zeichen entspricht. In Swift können Unicode-Zeichen mit mehreren Codepoints repräsentiert werden, was bedeutet, dass die Länge eines Strings von der Anzahl der Zeichenpunkte abhängen kann, die zur Darstellung der Zeichen verwendet werden.

# Siehe auch

- Swift Dokumentation zu Strings: https://developer.apple.com/documentation/swift/string
- SWIFT-Programmierhandbuch: https://swift.org/documentation/#the-swift-programming-language