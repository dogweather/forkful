---
title:                "Suchen und Ersetzen von Text"
html_title:           "Swift: Suchen und Ersetzen von Text"
simple_title:         "Suchen und Ersetzen von Text"
programming_language: "Swift"
category:             "Swift"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/de/swift/searching-and-replacing-text.md"
---

{{< edit_this_page >}}

## Warum
Warum sollte man sich mit der Suche und dem Austausch von Texten beschäftigen? Ein häufiger Anwendungsfall ist die Aktualisierung oder Korrektur von großen Textmengen, beispielsweise bei der Erstellung von Webseiten oder beim Bearbeiten von Computerdateien.

## Wie
Um einen Text in Swift zu suchen und zu ersetzen, gibt es verschiedene Möglichkeiten. Eine einfache Methode ist die Verwendung der `replacingOccurrences(of:with:)` Funktion. Hierbei wird der zu ersetzende Text als erster Parameter angegeben und der neue Text als zweiter Parameter. Beispiel:

```Swift
let text = "Hallo, mein Name ist John."
let newText = text.replacingOccurrences(of: "John", with: "Max")
print(newText) // Ausgabe: Hallo, mein Name ist Max.
```

Eine andere Möglichkeit ist die Verwendung von regulären Ausdrücken mit der `NSRegularExpression` Klasse. Diese erfordert etwas mehr Kenntnisse, kann jedoch auch komplexere Suchmuster abdecken. Beispiel:

```Swift
let text = "Ich habe heute 123€ ausgegeben."
let regex = try! NSRegularExpression(pattern: "[0-9]+€")
let newText = regex.stringByReplacingMatches(in: text, options: [], range: NSRange(text.startIndex..., in: text), withTemplate: "50€")
print(newText) // Ausgabe: Ich habe heute 50€ ausgegeben.
```

## Tiefergehender Einblick
Bei der Suche und dem Austausch von Texten ist es wichtig, auf Details wie Groß- und Kleinschreibung, Akzente und Sonderzeichen zu achten. Die `replacingOccurrences(of:with:)` Funktion bietet hierfür die Option `options`, welche mit `caseInsensitive` und `diacriticInsensitive` den Suchvorgang vereinfachen können.

Für fortgeschrittenere Fälle kann auch die `mutableString` Methode der `NSMutableString` Klasse verwendet werden, um den Text direkt zu manipulieren. Dies bietet mehr Flexibilität, erfordert jedoch auch mehr Aufwand.

## Siehe auch
- [Apple Dokumentation zu `replacingOccurrences(of:with:)`](https://developer.apple.com/documentation/foundation/nsstring/1417174-replacingoccurrences)
- [Apple Dokumentation zu regulären Ausdrücken](https://developer.apple.com/documentation/foundation/nsregularexpression)
- [Tutorial zu regulären Ausdrücken in Swift](https://www.tutorialspoint.com/swift/swift_regular_expressions.htm)
- [NSRegularExpression Playground von Swift.org](https://swift.org/blog/regular-expressions/)