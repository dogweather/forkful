---
title:                "Swift: Unterzeichenketten extrahieren"
simple_title:         "Unterzeichenketten extrahieren"
programming_language: "Swift"
category:             "Swift"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/de/swift/extracting-substrings.md"
---

{{< edit_this_page >}}

## Warum

Das Extrahieren von Teilstrings kann eine nützliche Funktion in der Swift Programmierung sein, da es es ermöglicht, nur einen Teil eines Texts zu extrahieren, anstatt den gesamten String zu verwenden. Dies kann besonders hilfreich sein, wenn man mit großen Texten arbeitet und nur bestimmte Teile davon benötigt.

## Wie geht man vor

Die Extraktion von Teilstrings kann mit der `substring` Funktion in Swift erreicht werden. Hier ist ein Beispielcode, der diesen Prozess veranschaulicht:

```Swift
let string = "Hello World"
let substring = string.substring(from: 6)
print(substring) // Output: World
```

Das `substring` muss mit dem Ausgangsstring und einer startenden Indexposition aufgerufen werden. In diesem Beispiel beginnt der Teilstring mit dem sechsten Zeichen, was dem Buchstaben "W" entspricht.

Es ist auch möglich, die Länge des Teilstrings anzugeben, wenn man nicht den gesamten Rest des Textes extrahieren möchte. In diesem Fall würde der Code folgendermaßen aussehen:

```Swift
let string = "Hello World"
let substring = string.substring(from: 6, length: 5)
print(substring) // Output: World
```

Das Argument `length` gibt an, wie viele Zeichen nach dem startenden Index extrahiert werden sollen.

## Tiefergehende Informationen

Man kann auch Teilstrings aus dem Anfang eines Strings extrahieren, indem man die `substring(to:)` Funktion verwendet. Diese nimmt lediglich die endende Indexposition als Argument:

```Swift
let string = "Hello World"
let substring = string.substring(to: 5)
print(substring) // Output: Hello
```

Wenn man bestimmte Zeichen in einem String als Teilstring extrahieren möchte, kann man die `range(of:)` Funktion verwenden, um die Indexpositionen dieser Zeichen zu finden und dann die `substring` Funktion anzuwenden.

## Siehe auch

- [Offizielle Swift Dokumentation](https://docs.swift.org/swift-book/LanguageGuide/StringsAndCharacters.html#ID283)
- [Extraktion von Teilstrings in Swift](https://www.hackingwithswift.com/example-code/strings/how-to-extract-a-substring-from-a-string)
- [Teilstrings mit range(of:) finden](https://www.hackingwithswift.com/example-code/strings/how-to-highlight-substrings-using-nsmutableattributedstring)