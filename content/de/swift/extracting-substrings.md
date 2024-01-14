---
title:                "Swift: Extrahieren von Teilstrings"
programming_language: "Swift"
category:             "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/de/swift/extracting-substrings.md"
---

{{< edit_this_page >}}

## Warum

Das Extrahieren von Teilstrings oder Teilabschnitten aus einem längeren Text kann in der Programmierung sehr nützlich sein. Zum Beispiel, wenn Sie Benutzereingaben prüfen müssen oder bestimmte Informationen aus einem Text extrahieren möchten. In der Swift Programmierung gibt es verschiedene Möglichkeiten, Teilstrings zu extrahieren, je nach Ihren individuellen Anforderungen.

## Wie geht das?

Um Teilstrings in Swift zu extrahieren, gibt es verschiedene Methoden, die Sie verwenden können. Eine Möglichkeit ist die Verwendung der `substring` Funktion. Hier ist ein Beispiel, wie Sie den dritten bis zum sechsten Buchstaben aus einem String extrahieren können:

```Swift
let text = "Willkommen"
let extractedText = text.substring(with: 2..<6) // ergebnis: "llko"
```

Sie können auch ein spezifisches Wort oder Zeichen als Trennungslinie verwenden, um Teilstrings zu extrahieren. Zum Beispiel, um den zweiten Namen aus einer Liste von Vor- und Nachnamen zu extrahieren:

```Swift
let names = "Maria, Anna, Max"
let separator = ", "
let secondName = names.components(separatedBy: separator)[1] // ergebnis: "Anna"
```

Es gibt auch eine Shorthand-Syntax für die `substring` Funktion, die es Ihnen ermöglicht, Teilstrings auf eine einfachere Art und Weise zu extrahieren:

```Swift
let text = "Apple"
let secondToLastChar = text[1...2] // ergebnis: "pp"
```

## Tiefergehende Informationen

Die `substring` Funktion ist nur eine von vielen Möglichkeiten, Teilstrings in Swift zu extrahieren. Es gibt auch andere Methoden wie `prefix`, `suffix` und `index` die je nach Situation nützlich sein können. Es ist wichtig, die verschiedenen Möglichkeiten zu kennen, um das richtige Werkzeug für die jeweilige Aufgabe auszuwählen.

## Siehe auch

- [Offizielle Swift-Dokumentation zu Teilstrings](https://developer.apple.com/documentation/swift/string/1784955-substring)
- [Weitere Beispiele für die Verwendung von Teilstrings in Swift](https://www.hackingwithswift.com/example-code/strings/how-to-extract-a-substring-from-a-string-using-indexes)
- [Ein kurzes Tutorial zur Verwendung von Shorthand-Notation für Teilstrings in Swift](https://medium.com/@johnsundell/swifts-string-refinements-3e72c0d753fe)