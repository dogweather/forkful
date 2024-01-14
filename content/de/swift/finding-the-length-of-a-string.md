---
title:                "Swift: Ermittlung der Länge von einem String"
simple_title:         "Ermittlung der Länge von einem String"
programming_language: "Swift"
category:             "Swift"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/de/swift/finding-the-length-of-a-string.md"
---

{{< edit_this_page >}}

# Warum

Das Finden der Länge eines Strings ist eine grundlegende Aufgabe beim Programmieren, die in vielen verschiedenen Anwendungsfällen benötigt wird. Zum Beispiel kann es nützlich sein, die Länge eines Benutzernamens zu überprüfen, um sicherzustellen, dass er den vorgegebenen Regeln entspricht. In diesem Blog-Beitrag lernen Sie, wie Sie die Länge eines Strings in Swift finden können.

## Wie

Um die Länge eines Strings in Swift zu finden, können Sie die `count` Funktion verwenden. Diese Funktion gibt die Anzahl der Zeichen in einem String zurück. Hier ist ein Beispielcode, der diese Funktion verwendet:

```Swift
let myString = "Hallo Welt"
print(myString.count)
```

Dieser Code würde die Zahl 11 ausgeben, da der String "Hallo Welt" aus 11 Zeichen besteht. Es ist auch möglich, die Länge eines Strings zu finden, der aus Zahlen besteht. In diesem Fall würde die `count` Funktion die Anzahl der Ziffern zurückgeben. Zum Beispiel:

```Swift
let myNumString = "1234"
print(myNumString.count)
```

Dieser Code würde die Zahl 4 ausgeben.

## Deep Dive

Die `count` Funktion gibt die Anzahl der Unicode-Symbole in einem String zurück, nicht die Anzahl der einzelnen Bytes. Das ist wichtig zu beachten, da manche Zeichen mehr als einen Byte benötigen. Zum Beispiel benötigt das Symbol "👋" vier Bytes, während das Symbol "A" nur einen benötigt.

Es gibt auch eine alternative Möglichkeit, die Länge eines Strings in Swift zu finden, nämlich mit der `characters` Eigenschaft. Diese Eigenschaft gibt eine Sammlung von einzelnen Zeichen im String zurück. Die Anzahl der Elemente in dieser Sammlung entspricht der Länge des Strings. Hier ist ein Beispielcode, der diese Methode verwendet:

```Swift
let myString = "Hallo Welt"
print(myString.characters.count)
```

Dieser Code würde ebenfalls die Zahl 11 ausgeben.

# Siehe auch

- Offizielle Swift Dokumentation: [Strings and Characters](https://docs.swift.org/swift-book/LanguageGuide/StringsAndCharacters.html)
- Stack Overflow Beitrag: [How to get the length of a string in Swift](https://stackoverflow.com/questions/24026510/how-do-i-get-the-length-of-a-string-in-swift)
- Ray Wenderlich Tutorial: [Strings and Characters in Swift](https://www.raywenderlich.com/144079/string-cheat-sheet-swift-3-0)