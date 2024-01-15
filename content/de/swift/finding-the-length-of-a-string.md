---
title:                "Die Länge eines Strings finden"
html_title:           "Swift: Die Länge eines Strings finden"
simple_title:         "Die Länge eines Strings finden"
programming_language: "Swift"
category:             "Swift"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/de/swift/finding-the-length-of-a-string.md"
---

{{< edit_this_page >}}

## Warum

Wenn du programmierst, wirst du mit Sicherheit häufig auf die Aufgabe stoßen, die Länge eines Strings zu bestimmen. Dies kann aus verschiedenen Gründen erforderlich sein, wie zum Beispiel die Validierung von Benutzereingaben oder das Formatieren von Text.

## Wie man die Länge eines Strings in Swift bestimmt

Um die Länge eines Strings in Swift zu bestimmen, gibt es verschiedene Methoden. Im Folgenden werden wir uns die gängigsten Möglichkeiten anschauen.

### Verwendung der `count` Methode

Eine der einfachsten Methoden, um die Länge eines Strings zu erhalten, ist die Verwendung der `count` Methode. Diese Methode gibt die Anzahl der Zeichen im String zurück, einschließlich Leerzeichen und Sonderzeichen.

```Swift
let string = "Hallo Welt!"
print(string.count) // Output: 11
```

### Verwendung der `unicodeScalars` Eigenschaft

Eine weitere Möglichkeit ist die Verwendung der `unicodeScalars` Eigenschaft, die eine Sequenz von Unicode-Code-Einheiten zurückgibt. Um die Länge des Strings zu erhalten, können wir einfach die `count` Methode auf die `unicodeScalars` Eigenschaft anwenden.

```Swift
let string = "Hallo Welt!"
print(string.unicodeScalars.count) // Output: 11
```

### Verwendung der `utf16` Eigenschaft

Ähnlich wie die `unicodeScalars` Eigenschaft gibt auch die `utf16` Eigenschaft eine Sequenz von Unicode-Code-Einheiten zurück. Die Verwendung der `utf16` Eigenschaft kann jedoch zu unterschiedlichen Ergebnissen führen, da sie einige Zeichen anders darstellt. Auch hier können wir die `count` Methode anwenden, um die Länge des Strings zu erhalten.

```Swift
let string = "Hallo Welt!"
print(string.utf16.count) // Output: 11
```

## Tiefgehende Analyse

Wenn es um die Verarbeitung von Strings in Swift geht, gibt es einige wichtige Konzepte zu verstehen. Eines davon ist die Unicode-Kodierung, die es ermöglicht, Zeichen aus verschiedenen Sprachen und Schriftsystemen darzustellen. Ein weiterer wichtiger Aspekt ist der Unterschied zwischen Zeichen und Bytes. Während Zeichen aus mehreren Bytes bestehen können, kann ein Byte nur ein Zeichen darstellen. Das ist der Grund, warum die verschiedenen Methoden, die wir oben betrachtet haben, zu unterschiedlichen Ergebnissen führen können. 

## Siehe auch

- [Swift Dokumentation zu Strings](https://docs.swift.org/swift-book/LanguageGuide/StringsAndCharacters.html)
- [Stack Overflow: Swift - Wie erhalte ich die Länge eines Strings?](https://stackoverflow.com/questions/24086161/swift-how-do-i-count-the-number-of-characters-in-a-string)