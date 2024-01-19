---
title:                "Suchen und Ersetzen von Text"
html_title:           "C#: Suchen und Ersetzen von Text"
simple_title:         "Suchen und Ersetzen von Text"
programming_language: "Swift"
category:             "Swift"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/de/swift/searching-and-replacing-text.md"
---

{{< edit_this_page >}}

# Suchen und Ersetzen in Swift

## Was & Warum?
Eine Suche und Ersetzung von Text ermöglicht es, bestimmte Teile eines Strings zu finden und auszutauschen. Dies ist in vielen Anwendungsfällen nützlich, wie z.B. bei der Datenbereinigung oder Formatänderung.

## So geht's:
Im folgenden sehen wir, wie Text in Swift gesucht und ersetzt wird. Wir verwenden die `replacingOccurrences` Methode der `String` Klasse.

```Swift
let originalString = "Hallo Welt"
let replacedString = originalString.replacingOccurrences(of: "Welt", with: "Swift")
print(replacedString)  // Ausgabe: Hallo Swift
```
In diesem Codeblock erstellen wir zuerst einen String "Hallo Welt". Dann ersetzen wir "Welt" durch "Swift" und erhalten "Hallo Swift".

## Vertiefung
Historisch gesehen basiert die `replacingOccurrences` Methode auf der Regular Expression Technologie, die ursprünglich von Unix entwickelt wurde. Alternativen zur `replacingOccurrences` Methode wären z.B. die Erstellung eigener Funktionen, allerdings ist `replacingOccurrences` aufgrund ihrer Einfachheit und Effizienz in den meisten Situationen vorzuziehen. Die Methode durchläuft intern den Text, sucht nach Übereinstimmungen und ersetzt diese durch den angegebenen neuen Text.

## Siehe auch
Für weiterführende Informationen und zusätzliche Kontext siehe die folgenden Ressourcen:

1. [Swift String Dokumentation](https://developer.apple.com/documentation/swift/string)
2. [Swift Regular Expression Tutorial](https://nshipster.com/swift-regular-expressions/)
3. [Text Manipulation in Swift](https://www.hackingwithswift.com/articles/141/8-powerful-swift-features-that-sometimes-confuse-beginners)