---
title:                "Schreiben auf Standardfehler"
html_title:           "Swift: Schreiben auf Standardfehler"
simple_title:         "Schreiben auf Standardfehler"
programming_language: "Swift"
category:             "Swift"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/de/swift/writing-to-standard-error.md"
---

{{< edit_this_page >}}

# Warum?

In der Welt der Programmierung gibt es immer wieder Situationen, in denen Fehler auftreten können. Diese Fehler werden oft von dem Programmierer behandelt und auf eine bestimmte Weise ausgegeben. Eine Möglichkeit, dies zu tun, ist das Schreiben auf den Standardfehler (Standard Error). In diesem Artikel werden wir besprechen, warum und wie man auf den Standardfehler schreibt und einige tiefergehende Informationen über diesen Prozess geben.

## Wie man auf den Standardfehler schreibt

Das Schreiben auf den Standardfehler ist eine nützliche Technik, um Fehler in einem Programm zu behandeln. Um auf den Standardfehler zu schreiben, muss man lediglich die Funktion `print(_:to:)` verwenden und den zu schreibenden Text und den Standardfehler als Argumente übergeben. Hier ist ein Beispiel in Swift:

```
let errorMessage = "Dies ist eine Fehlermeldung"
print(errorMessage, to: &standardError)
```

Wenn dieses Codebeispiel ausgeführt wird, wird die Fehlermeldung auf dem Terminal ausgegeben, zusammen mit anderen Ausgaben des Programms. Das Schreiben auf den Standardfehler ist besonders nützlich, wenn man ein Skript schreibt oder wenn das Programm keine grafische Benutzeroberfläche hat.

## Tiefergehender Einblick

In der Vergangenheit war es üblich, auf den Standardausgang (Standard Output) zu schreiben, um Fehlermeldungen zu behandeln. Allerdings kann die Verwendung des Standardausgangs missverstanden werden, da er oft für normale Ausgaben des Programms verwendet wird. Das Schreiben auf den Standardfehler bietet eine klare Trennung zwischen den normalen Ausgaben und den Fehlermeldungen.

Eine weitere wichtige Sache, die man beachten sollte, ist, dass das Schreiben auf den Standardfehler asynchron erfolgt. Das bedeutet, dass der Code, der nach dem Schreiben auf den Standardfehler geschrieben wird, möglicherweise vor dem Schreiben auf den Standardfehler ausgeführt wird. Um sicherzustellen, dass das Schreiben auf den Standardfehler beendet ist, sollte man die Funktion `flush()` aufrufen.

## Siehe auch

Hier sind einige nützliche Links, um mehr über das Schreiben auf den Standardfehler in Swift zu erfahren:

- [Apple Documentation on Standard Error](https://developer.apple.com/documentation/foundation/standarderror)
- [Stack Overflow Thread on Writing to Standard Error in Swift](https://stackoverflow.com/questions/31927311/write-string-to-stderr)
- [Swift Programming Language Guide](https://docs.swift.org/swift-book/LanguageGuide/TheBasics.html)