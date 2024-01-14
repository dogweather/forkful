---
title:                "Swift: Verwendung von regulären Ausdrücken"
programming_language: "Swift"
category:             "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/de/swift/using-regular-expressions.md"
---

{{< edit_this_page >}}

# Warum

Regular Expressions bieten eine leistungsstarke Möglichkeit, Muster in Strings zu suchen und zu manipulieren. Sie sind besonders nützlich für die Verarbeitung von Texten in Apps oder Skripten.

# Wie man es benutzt

Die Verwendung von regulären Ausdrücken in Swift ist recht einfach. Sie können die "NSRegularExpression" Klasse verwenden, um einen Ausdruck zu erstellen, der dann in einer String-Variable gesucht und ersetzt werden kann.

```Swift
let text = "Hallo Welt! Willkommen in Swift!"
let pattern = "Hallo"
let regex = try NSRegularExpression(pattern: pattern)
let result = regex.stringByReplacingMatches(in: text, options: [], range: NSRange(location: 0, length: text.count), withTemplate: "Guten Tag")
print(result)
// Ausgabe: Guten Tag Welt! Willkommen in Swift!
```

In diesem Beispiel wird der Ausdruck "Hallo" durch "Guten Tag" ersetzt. Es sind jedoch noch viel komplexere Ausdrücke möglich, um zum Beispiel bestimmte Zeichen oder Wörter zu finden.

# Tiefere Einblicke

Um reguläre Ausdrücke effektiv zu nutzen, sollten Sie sich mit der Syntax vertraut machen. Sie können spezielle Zeichen verwenden, um bestimmte Muster zu definieren, wie zum Beispiel:

- "." für jedes einzelne Zeichen
- "^" am Anfang eines Ausdrucks, um von dort aus zu suchen
- "$" am Ende eines Ausdrucks, um zu sehen, ob es dort endet
- "+" um anzuzeigen, dass ein Zeichen oder Ausdruck mindestens einmal vorhanden sein muss
- "*" um anzuzeigen, dass ein Zeichen oder Ausdruck beliebig oft vorhanden sein kann
- und vieles mehr.

Eine detaillierte Liste der verfügbaren Zeichen und deren Verwendung finden Sie in dieser [Swift-Befehlsreferenz](https://developer.apple.com/documentation/foundation/nsregularexpression).

# Siehe auch

- [Offizielle Swift-Dokumentation zu "NSRegularExpression"](https://developer.apple.com/documentation/foundation/nsregularexpression)
- [Ein Tutorial für die Verwendung von regulären Ausdrücken in Swift](https://www.raywenderlich.com/86205/nsregularexpression-swift-tutorial)
- [Eine umfassende Anleitung zu regulären Ausdrücken für fortgeschrittenere Anwendungen](https://www.regular-expressions.info/)