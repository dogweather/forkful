---
title:                "Textsuche und -ersetzung"
html_title:           "Swift: Textsuche und -ersetzung"
simple_title:         "Textsuche und -ersetzung"
programming_language: "Swift"
category:             "Swift"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/de/swift/searching-and-replacing-text.md"
---

{{< edit_this_page >}}

## Was & Warum?

Suchen und Ersetzen ist eine häufige Aufgabe in der Programmierung. Sie bezieht sich auf das Durchsuchen von Text nach bestimmten Zeichenfolgen und dann auf das Ersetzen dieser Zeichenfolgen durch eine andere. Programmierer verwenden diese Technik, um schnell bestimmte Wörter oder Codes in ihrem Programm zu finden und zu ändern, anstatt manuell durch den gesamten Code zu suchen.

## Wie geht's:

Eine Möglichkeit, Text in Swift zu suchen und zu ersetzen, ist die Verwendung der built-in Funktion `replacingOccurrences(of:with:)`. Hier ist ein Beispiel:

```Swift
let str = "Hallo, wie geht es dir?"
let newStr = str.replacingOccurrences(of: "hallo", with: "Hallo")
print(newStr)

// Ausgabe: "Hallo, wie geht es dir?"
```

In diesem Beispiel ersetzt die Funktion `replacingOccurrences(of:with:)` alle Vorkommen des Wortes "hallo" durch "Hallo" und gibt den neuen String aus. Sie können auch mehrere Ersetzungen in einem String durchführen, indem Sie die Funktion mehrmals aufrufen.

## Tiefer Eintauchen:

Die Technik des Suchens und Ersetzens gibt es schon seit langem und wird in verschiedenen Programmen verwendet, nicht nur in der Programmierung. Es gibt auch alternative Methoden, um Text in Swift zu suchen und zu ersetzen, wie z.B. die Verwendung von regulären Ausdrücken.

Die Funktion `replacingOccurrences(of:with:)` verwendet standardmäßig das Case-Sensitive-Matching, was bedeutet, dass sie zwischen Groß- und Kleinschreibung unterscheidet. Um dies zu vermeiden, können Sie den Parameter `options` mit dem Wert `caseInsensitive` angeben. Es gibt auch andere Optionen wie `literal`, `numeric`, `anchored` usw., die mehr Kontrolle über das Suchen und Ersetzen bieten.

## Siehe auch:

Wenn Sie mehr über das Suchen und Ersetzen von Text in Swift erfahren möchten, können Sie die offizielle Swift-Dokumentation besuchen oder diese [Tutorial-](https://www.raywenderlich.com/139591/regular-expressions-tutorial-ios-swift) und [Video-](https://www.youtube.com/watch?v=VzBkcFJChuE) Ressourcen erkunden.