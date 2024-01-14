---
title:    "Swift: Textsuche und Ersetzen"
keywords: ["Swift"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/de/swift/searching-and-replacing-text.md"
---

{{< edit_this_page >}}

## Warum

Suchen und Ersetzen von Text ist eine häufige Aufgabe beim Programmieren. Es kann helfen, Fehler zu finden und zu beheben, den Code lesbarer zu machen oder bestimmte Teile des Codes schnell zu ändern.

## Wie

Um Text in Swift zu suchen und zu ersetzen, gibt es mehrere Optionen. Die einfachste Möglichkeit ist die Verwendung der `replacingOccurrences`-Funktion, die einen String durch einen anderen ersetzt.

```Swift
let sentence = "Das ist ein Beispieltext."
let newSentence = sentence.replacingOccurrences(of: "Beispieltext", with: "anderer Text")
print(newSentence)
// Output: Das ist ein anderer Text.
```

Man kann auch reguläre Ausdrücke verwenden, um Text zu suchen und zu ersetzen. Dazu muss man die `replacingOccurrences`-Funktion mit der Option `regularExpression` aufrufen und den regulären Ausdruck als Argument übergeben.

```Swift
let sentence = "Der Code ist 123 Zeichen lang."
let newSentence = sentence.replacingOccurrences(of: "[0-9]+", with: "", options: .regularExpression)
print(newSentence)
// Output: Der Code ist Zeichen lang.
```

## Deep Dive

Bei der Suche und dem Ersetzen von Text mittels regulärer Ausdrücke gibt es eine Menge zu beachten. Man sollte sich mit den Grundlagen und Syntax von regulären Ausdrücken auseinandersetzen, um effektive Ausdrücke zu schreiben. Eine gute Möglichkeit, dies zu tun, ist die Verwendung von Online-Tools wie regex101 oder RegExr, die einem helfen, die Ausdrücke Schritt für Schritt aufzubauen und zu testen.

Es gibt auch verschiedene Optionen und Flags, die verwendet werden können, um das Verhalten der Suche und des Ersatzes zu beeinflussen. Zum Beispiel kann man mithilfe des Flags `caseInsensitive` die Suche case-insensitive machen, was bedeutet, dass Groß- und Kleinschreibung ignoriert werden.

## Siehe auch

- [Apple Swift Dokumentation - Strings and Characters](https://docs.swift.org/swift-book/LanguageGuide/StringsAndCharacters.html)
- [Regular-Expressions.info](https://regular-expressions.info/)