---
title:                "Swift: Die Länge eines Strings finden"
programming_language: "Swift"
category:             "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/de/swift/finding-the-length-of-a-string.md"
---

{{< edit_this_page >}}

## Warum

Möglicherweise fragen Sie sich, warum Sie sich überhaupt Gedanken darüber machen sollten, wie Sie die Länge einer Zeichenkette in Swift finden können. Nun, die Antwort ist einfach - jeder Programmierer, der schon einmal mit Zeichenketten gearbeitet hat, weiß, wie wichtig es ist, ihre Länge zu kennen. Indem Sie die Länge einer Zeichenkette finden, können Sie bestimmte Operationen wie das Überprüfen der Gültigkeit oder das Zählen von Zeichen durchführen.

## Wie

Um die Länge einer Zeichenkette in Swift zu finden, können Sie die Methode `count` verwenden. Schauen wir uns ein Beispiel an:

```Swift
let string = "Hallo Welt"
print(string.count)
```

Dies wird die Ausgabe `11` erzeugen, da die Zeichenkette "Hallo Welt" 11 Zeichen hat.

Sie können auch die `count` Methode auf Substrings anwenden, um die Länge eines Teils der Zeichenkette zu finden. Hier ist ein Beispiel:

```Swift
let string = "Hallo Welt"
let substring = string[..<string.index(string.startIndex, offsetBy: 5)]
print(substring.count)
```

Dies wird die Ausgabe `5` erzeugen, da der Substring "Hallo" 5 Zeichen hat.

## Deep Dive

Wenn Sie tiefer in das Konzept der Zeichenkettenlänge in Swift eintauchen möchten, können Sie sich mit den Unterschieden zwischen `count` und `length` auseinandersetzen. Während `count` die Anzahl der Unicode-Skalarwerte in einer Zeichenkette zählt, gibt `length` die Anzahl der UTF-16-Codewerte zurück. Dies kann zu unterschiedlichen Ergebnissen führen, wenn Sie nicht-ASCII-Zeichen in Ihrer Zeichenkette haben. Es ist wichtig zu wissen, welches dieser beiden Methoden für Ihre spezifische Anwendung relevant ist.

## Siehe auch

- [Die offizielle Swift-Dokumentation zu Zeichenketten](https://docs.swift.org/swift-book/LanguageGuide/StringsAndCharacters.html)
- [Eine detaillierte Erklärung zu Zeichenkettenlängen in Swift](https://learnappmaking.com/string-length-swift-how-to/)