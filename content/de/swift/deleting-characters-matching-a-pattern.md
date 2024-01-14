---
title:    "Swift: Entfernen von Zeichen, die einem Muster entsprechen."
keywords: ["Swift"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/de/swift/deleting-characters-matching-a-pattern.md"
---

{{< edit_this_page >}}

##Warum

In diesem Blogpost geht es darum, wie man in Swift Zeichen löschen kann, die einem bestimmten Muster entsprechen. Dies kann besonders nützlich sein, wenn man beispielsweise Benutzereingaben auf bestimmte Formate prüfen möchte.

##Anleitung

Um in Swift Zeichen zu löschen, die einem bestimmten Muster entsprechen, kann man die Funktion `replacingOccurrences` verwenden. Diese Funktion erwartet zwei Argumente: Das erste ist das Muster, das gelöscht werden soll, und das zweite ist der String, in dem das Muster gefunden werden soll. 

```Swift
let input = "Abc123"
let output = input.replacingOccurrences(of: "[A-Za-z]", with: "", options: .regularExpression)
print(output) // 123
```

In diesem Beispiel wird der String "Abc123" in "123" umgewandelt, indem alle Buchstaben durch leere Strings ersetzt werden. Das Muster `[A-Za-z]` steht dabei für alle Groß- und Kleinbuchstaben im Alphabet.

##Tiefere Einblicke

Für die Funktion `replacingOccurrences` gibt es verschiedene Optionen, die das Löschen von Zeichen noch flexibler machen. Zum Beispiel kann man mit der Option `.caseInsensitive` auch Groß- und Kleinschreibung ignorieren. Oder mit der Option `.anchored` kann man festlegen, dass das Muster nur am Anfang des Strings gelöscht werden soll. 

Es ist außerdem möglich, mehrere Muster nacheinander zu löschen, indem man die Funktion mehrmals hintereinander aufruft. 

```Swift
let input = "abc123def"
let output = input.replacingOccurrences(of: "[A-Za-z]", with: "", options: .regularExpression)
                .replacingOccurrences(of: "[0-9]", with: "", options: .regularExpression)
print(output) // def 
```

Das erste Aufrufen der Funktion löscht alle Buchstaben, das zweite alle Zahlen. So erhält man am Ende nur noch den String "def". 

##Siehe auch

- [Offizielle Dokumentation zu `replacingOccurrences`](https://developer.apple.com/documentation/foundation/nsstring/1413232-replacingoccurrences)
- [Beispielprojekt auf GitHub](https://github.com/example/swift-replacing-occurrences)
- [Diskussion zum Thema auf Stack Overflow](https://stackoverflow.com/questions/43231526/deleting-special-characters-from-string-with-swift)