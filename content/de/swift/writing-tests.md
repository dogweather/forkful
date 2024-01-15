---
title:                "Tests schreiben"
html_title:           "Swift: Tests schreiben"
simple_title:         "Tests schreiben"
programming_language: "Swift"
category:             "Swift"
tag:                  "Testing and Debugging"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/de/swift/writing-tests.md"
---

{{< edit_this_page >}}

## Warum

Tests spielen eine wichtige Rolle in der Softwareentwicklung, da sie helfen, Codequalität und Funktionalität zu überprüfen. Die Verwendung von Tests ermöglicht es Entwicklern, sicherer und effizienter Änderungen an ihrem Code vorzunehmen, da sie wissen, dass ihre Tests den erwarteten Output liefern.

## Wie man Tests schreibt

Um Tests in Swift zu schreiben, müssen Sie zuerst das XCTest Framework importieren. Dann können Sie spezifische Testfälle erstellen, die Methoden aus Ihrer Codebasis aufrufen und den erwarteten Output mit dem tatsächlichen Output vergleichen.

```Swift
import XCTest

// Testklasse für eine Funktion, die zwei Zahlen addiert
class CalculatorTests: XCTestCase {
    // Testfall für die Funktion add()
    func testAdd() {
        // Eingabe
        let number1 = 5
        let number2 = 10
        
        // Erwarteter Output
        let expected = 15
        
        // Tatsächlicher Output
        let result = add(number1, number2)
        
        // Assert, um zu überprüfen, ob der erwartete Output dem tatsächlichen Output entspricht
        XCTAssert(result == expected, "Die add() Funktion sollte \(expected) zurückgeben, aber stattdessen wurde \(result) zurückgegeben.")
    }
}
```

Um Ihre Tests auszuführen, können Sie entweder die "Test" -Option in Xcode auswählen oder das Command Line Tool "xcodebuild" verwenden.

## Tiefere Einblicke

Beim Schreiben von Tests gibt es verschiedene Konzepte und Techniken, die es zu beachten gilt. Dazu gehören zum Beispiel das Mocking von Objekten, um Isolation zu erreichen, die Verwendung von Testdaten und die Verwendung von strukturierten Testfällen. Es ist auch wichtig zu bedenken, dass Tests regelmäßig ausgeführt und gewartet werden müssen, um sicherzustellen, dass sie immer noch relevant und aussagekräftig sind.

## Siehe auch

- [Offizielle Dokumentation zu XCTest](https://developer.apple.com/documentation/xctest)
- [Artikel über Unit-Tests in Swift](https://www.swiftbysundell.com/articles/unit-testing-in-swift/)
- [Video-Tutorial über Testing in Swift](https://www.youtube.com/watch?v=Rj2EJLSiDD8&t=561s)