---
title:                "Swift: Suchen und Ersetzen von Text"
simple_title:         "Suchen und Ersetzen von Text"
programming_language: "Swift"
category:             "Swift"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/de/swift/searching-and-replacing-text.md"
---

{{< edit_this_page >}}

## Warum

Wer sich mit der Programmiersprache Swift beschäftigt, wird schnell merken, dass das Suchen und Ersetzen von Text ein wichtiger Bestandteil der Entwicklung ist. Durch das einfache Ersetzen von bestimmten Textstellen können Fehler behoben und Code optimiert werden.

## How To

Um Text in Swift zu suchen und zu ersetzen, gibt es mehrere Möglichkeiten. Eine davon ist die Verwendung der `replacingOccurrences(of:with:)` Methode. Hier ein Beispiel:

```Swift
let text = "Mein Name ist Maria."
let newText = text.replacingOccurrences(of: "Maria", with: "Anna")
print(newText)
```

Dies gibt als Output "Mein Name ist Anna." aus. Die Methode ersetzt hier den Text "Maria" mit "Anna". 

## Deep Dive

Die `replacingOccurrences(of:with:)` Methode ermöglicht es, nicht nur einzelne Textstellen zu ersetzen, sondern auch mehrere. Beispielsweise können auch alle Vorkommen eines bestimmten Wortes in einem String gleichzeitig ersetzt werden. Zudem ist die Methode in der Lage, Groß- und Kleinschreibung zu ignorieren und auch mit regulären Ausdrücken zu arbeiten.

## Siehe auch

- [The Swift Programming Language](https://docs.swift.org/swift-book/LanguageGuide/StringsAndCharacters.html#ID289)
- [NSHipster: String](https://nshipster.com/string/)
- [AppCoda: Swift String Manipulation](https://www.appcoda.com/swift-string-manipulation/)