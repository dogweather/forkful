---
title:                "Swift: Verbinden von Zeichenketten"
simple_title:         "Verbinden von Zeichenketten"
programming_language: "Swift"
category:             "Swift"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/de/swift/concatenating-strings.md"
---

{{< edit_this_page >}}

## Warum

Das Verketten von Strings ist ein häufiges Szenario in der Programmierung, bei dem mehrere Strings miteinander verbunden werden müssen, um einen längeren Text zu erstellen. Dies kann zum Beispiel nützlich sein, um personalisierte Benachrichtigungen oder dynamische Inhalte zu erstellen. In Swift gibt es verschiedene Methoden, um Strings zu kombinieren, und im Folgenden werden wir uns genauer mit diesem Thema befassen.

## Wie Geht Man Vor

Um Strings in Swift zu verketten, können wir verschiedene Methoden verwenden. Eine Möglichkeit ist die Verwendung des "+=" Operators, der zwei Strings miteinander verbindet und das Ergebnis dem ersten String zuweist. Zum Beispiel:

```Swift
var name = "Max"
var greeting = "Hallo"
greeting += name // Das Ergebnis ist "Hallo Max"
```

Eine weitere gängige Methode ist die Verwendung der "string interpolation", bei der Variablen oder Konstanten in einen String eingefügt werden. Dies geschieht durch Voranstellen eines Backslashes (\) gefolgt von den geschweiften Klammern ({}) und dem Namen der Variable oder Konstanten. Zum Beispiel:

```Swift
var name = "Max"
var greeting = "Hallo \(name)" // Das Ergebnis ist "Hallo Max"
```

Es ist auch möglich, die Funktion "joined(separator:)" zu verwenden, um mehrere Strings miteinander zu verbinden und einen angegebenen Trenner zwischen ihnen zu platzieren. Zum Beispiel:

```Swift
let fruits = ["Apfel", "Banane", "Erdbeere"]
let joinedFruits = fruits.joined(separator: ", ") // Das Ergebnis ist "Apfel, Banane, Erdbeere"
```

Neben diesen Methoden gibt es noch weitere Möglichkeiten, Strings in Swift zu verketten, die alle auf der offiziellen Swift Dokumentation näher erläutert werden.

## Tieferer Einblick

Wenn es um die Verkettung von Strings geht, ist es wichtig zu beachten, dass Strings in Swift unveränderliche Objekte sind. Das bedeutet, dass ein String nach seiner Erstellung nicht mehr geändert werden kann. Stattdessen wird bei jeder Verkettung eines Strings ein neues String-Objekt erstellt. Dies kann insbesondere bei der Verkettung von vielen Strings zu Performanceproblemen führen.

Um diesem Problem entgegenzuwirken, empfiehlt es sich, den "StringBuilder" zu verwenden, der in anderen Sprachen wie Java oder C# üblich ist. Dabei handelt es sich um eine strukturähnliche Klasse, die mehrere Strings in einem Objekt speichern und nur bei Bedarf bei der Verkettung neue Objekte erstellen kann.

Außerdem ist es wichtig zu beachten, dass in Swift Unicode-Unterstützung integriert ist, was bedeutet, dass Strings alle möglichen Zeichen aus allen Sprachen enthalten können. Dies kann manchmal zu Problemen führen, wenn unterschiedliche Zeichencodierungen verwendet werden oder wenn Zeichen mit besonderen Eigenschaften wie Emojis verwendet werden.

## Siehe Auch

- Offizielle Swift Dokumentation: https://docs.swift.org/swift-book/LanguageGuide/StringsAndCharacters.html
- Entwickler-Forum: https://swiftforums.com/t/concatenate-strings/33/2
- Stack Overflow: https://stackoverflow.com/questions/9280542/how-to-concatenate-strings-in-swift