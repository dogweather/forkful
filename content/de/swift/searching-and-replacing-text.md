---
title:                "Swift: Suchen und Ersetzen von Text"
programming_language: "Swift"
category:             "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/de/swift/searching-and-replacing-text.md"
---

{{< edit_this_page >}}

# Warum

Suchen und Ersetzen ist ein grundlegender Aspekt der Textmanipulation in der Programmierung. Es kann Ihnen dabei helfen, effizienter zu programmieren, indem Sie Text schnell ändern und anpassen können.

# Wie

Die Suche und Ersetzung von Text in Swift ist einfach und kann auf verschiedene Arten durchgeführt werden. Hier sind einige Beispiele:

```
Swift 
let text = "Hallo Welt!"
print(text.replacingOccurrences(of: "Hallo", with: "Hallo, liebe"))
```

Ausgabe: "Hallo, liebe Welt!"

```
Swift 
let text = "Ich lerne Swift!"
print(text.replacingCharacters(in: text.startIndex..<text.index(text.startIndex, offsetBy: 5), with: "Wir sind"))
```

Ausgabe: "Wir sind Swift!"

```
Swift 
let text = "Dies ist ein Beispiel"
print(text.replacingOccurrences(of: "e", with: "i", options: .caseInsensitive, range: text.startIndex..<text.index(text.startIndex, offsetBy: 14)))
```

Ausgabe: "Dis ist in Bispil"

Sie können auch reguläre Ausdrücke verwenden, um Text in Swift zu suchen und zu ersetzen. Hier ist ein Beispiel, bei dem alle Zahlen in einer Zeichenkette entfernt werden:

```
Swift 
let text = "1234 Hallo 5678 Welt"
let numbersRegex = try! NSRegularExpression(pattern: "[0-9]", options: [])
let modifiedText = numbersRegex.stringByReplacingMatches(in: text, options: [], range: NSRange(location: 0, length: text.utf16.count), withTemplate: "")
print(modifiedText)
```

Ausgabe: " Hallo  Welt"

# Deep Dive

In Swift gibt es viele verschiedene Methoden für die Suche und Ersetzung von Text, die je nach Bedarf eingesetzt werden können. Sie können auch komplexe Regeln und Muster für die Suche verwenden, um genau den Text zu erhalten, den Sie benötigen.

Es ist wichtig zu beachten, dass die Suche und Ersetzung von Text in Swift immer auf Strings basiert, die als unveränderliche Zeichenketten behandelt werden. Dies bedeutet, dass jede Änderung an einem String tatsächlich eine neue Kopie des Strings erzeugt, anstatt den ursprünglichen String zu ändern.

# Siehe auch

- [Offizielle Dokumentation von Apple zu string manipulation in Swift](https://developer.apple.com/documentation/foundation/nsstring)
- [Swift Regular Expressions Tutorial](https://www.raywenderlich.com/6734-regular-expressions-tutorial-getting-started)