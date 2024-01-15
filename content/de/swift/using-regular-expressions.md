---
title:                "Die Verwendung von regulären Ausdrücken"
html_title:           "Swift: Die Verwendung von regulären Ausdrücken"
simple_title:         "Die Verwendung von regulären Ausdrücken"
programming_language: "Swift"
category:             "Swift"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/de/swift/using-regular-expressions.md"
---

{{< edit_this_page >}}

## Warum

Warum sollte man sich mit regulären Ausdrücken beschäftigen? Nun, sie sind ein äußerst leistungsstarkes Werkzeug, das es uns ermöglicht, komplexe Such- und Ersetzungsvorgänge in Texten schnell und effizient durchzuführen. Sie sind besonders nützlich für Entwickler, die mit Parsing, Datenvalidierung oder Textverarbeitung zu tun haben.

## Wie man reguläre Ausdrücke in Swift verwendet

Um reguläre Ausdrücke in Swift zu verwenden, müssen wir zuerst die `NSRegularExpression`-Klasse importieren. Dann können wir eine Instanz dieser Klasse erstellen und sie mit dem Muster füllen, nach dem wir suchen möchten.

```Swift
import Foundation

// Erstelle eine Instanz von NSRegularExpression
let muster = "RegEx"
let regex = try! NSRegularExpression(pattern: muster, options: .caseInsensitive)

// Lege den zu durchsuchenden String fest
let text = "RegEx ist großartig!"

// Suche nach Übereinstimmungen im Text
let matches = regex.matches(in: text, options: [], range: NSRange(location: 0, length: text.utf16.count))

// Gehe durch jede Übereinstimmung und zeige sie an
for match in matches {
    let range = match.range
    print("Gefunden bei Index \(range.location) mit Länge \(range.length)")
}
```

In diesem Beispiel verwenden wir das Muster "RegEx" und suchen nach allen Übereinstimmungen im Text, unabhängig von Groß- und Kleinschreibung. Die Ausgabe wird folgendermaßen aussehen:

```
Gefunden bei Index 0 mit Länge 5
Gefunden bei Index 12 mit Länge 5
```

## Tiefer Einblick

Reguläre Ausdrücke können auch verwendet werden, um Text zu ersetzen. Mit der Methode `stringByReplacingMatches(in:options:range:withTemplate:)` können wir alle Übereinstimmungen im Text durch einen anderen String ersetzen.

```Swift
let neuerText = regex.stringByReplacingMatches(in: text, options: [], range: NSRange(location: 0, length: text.utf16.count), withTemplate: "Regular Expressions")
print(neuerText)
```

In diesem Beispiel ersetzen wir alle Übereinstimmungen von "RegEx" durch "Regular Expressions" und erhalten folgende Ausgabe:

```
Regular Expressions ist großartig!
```

Es gibt auch viele nützliche Metazeichen, mit denen wir unsere regulären Ausdrücke noch leistungsfähiger machen können. Zum Beispiel `+` für ein oder mehrere Vorkommen, `?` für null oder ein Vorkommen und `|` für Alternative.

## Siehe auch

- [Apple Dokumentation über reguläre Ausdrücke in Swift](https://developer.apple.com/documentation/foundation/nsregularexpression)
- [Einführung in reguläre Ausdrücke von RegEx Cookbook](https://www.regular-expressions.info/tutorial.html)
- [Reguläre Ausdrücke üben mit RegexOne](https://regexone.com/)