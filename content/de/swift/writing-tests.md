---
title:    "Swift: Tests schreiben"
keywords: ["Swift"]
---

{{< edit_this_page >}}

# Warum

Tests sind ein wichtiger Bestandteil der Entwicklung in Swift. Sie ermöglichen es Entwicklern, ihre Codequalität zu verbessern, Fehler frühzeitig zu erkennen und die Gesamtstabilität ihrer Anwendung zu erhöhen. Indem Sie solche Tests schreiben, können Sie auch sicherstellen, dass zukünftige Änderungen oder Updates Ihre App nicht unerwartet beeinträchtigen werden.

# Wie

Um Tests in Swift zu schreiben, müssen Sie zuerst eine sogenannte "Test Suite" erstellen. Diese Suite enthält alle Tests, die Sie durchführen möchten. Hier ist ein Beispiel einer einfachen Test Suite:

```Swift
import XCTest

class MyTests: XCTestCase {
    func testAddition() {
        let result = 2 + 2
        XCTAssertEqual(result, 4, "2 + 2 sollte 4 ergeben")
    }
}
```

In diesem Beispiel wird eine Testklasse namens "MyTests" erstellt und eine Funktion namens "testAddition" definiert. In dieser Funktion wird eine einfache Addition durchgeführt und dann mit der erwarteten Ausgabe verglichen. Die Funktion "XCTAssertEqual" überprüft, ob beide Werte gleich sind und gibt andernfalls eine Fehlermeldung aus.

Sie können auch Tests für Funktionen schreiben, die Parameter nehmen. Hier ist ein Beispiel dafür:

```Swift
func multiply(_ a: Int, by b: Int) -> Int {
    return a * b
}

func testMultiply() {
    let result = multiply(3, by: 4)
    XCTAssertEqual(result, 12, "3 * 4 sollte 12 ergeben")
}
```

In diesem Beispiel wird die Funktion "multiply" getestet, indem sie mit den Werten 3 und 4 aufgerufen wird. Wie zuvor wird mit "XCTAssertEqual" überprüft, ob die erwartete Ausgabe erzielt wird.

# Tiefere Einblicke

Es ist wichtig, auch negative Testfälle zu schreiben, um sicherzustellen, dass Ihr Code auch in unerwarteten Situationen richtig funktioniert. Sie können dies mit "XCTAssertNotEqual" tun, um sicherzustellen, dass zwei Werte nicht gleich sind.

Sie können auch "XCTAssertThrowsError" verwenden, um zu überprüfen, ob eine bestimmte Funktion tatsächlich einen Fehler wirft, wenn sie mit ungültigen Eingaben aufgerufen wird.

Es gibt noch viele weitere Möglichkeiten, Tests in Swift zu schreiben, die hier nicht alle aufgelistet werden können. Es ist wichtig, sich mit den verschiedenen verfügbaren Testmethoden vertraut zu machen und sie entsprechend Ihren Bedürfnissen anzuwenden.

# Siehe auch

- [Offizielle Dokumentation zu XCTest](https://developer.apple.com/documentation/xctest)
- [Swift Unit Testing Tutorial von Ray Wenderlich](https://www.raywenderlich.com/960290-ios-unit-testing-and-ui-testing-tutorial)
- [Warum und wie man in Swift testen sollte](https://www.marcc.com/why-and-how-to-test-swift-code)