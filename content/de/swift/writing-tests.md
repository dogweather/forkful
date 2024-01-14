---
title:    "Swift: Tests schreiben"
keywords: ["Swift"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/de/swift/writing-tests.md"
---

{{< edit_this_page >}}

Warum Tests schreiben?

Tests sind ein unerlässliches Werkzeug für jeden Programmierer, der qualitativ hochwertigen Code schreiben möchte. Durch das Schreiben von Tests können Bugs frühzeitig erkannt und behoben werden, was Zeit und Geld sparen kann. Außerdem helfen Tests dabei, das Vertrauen in den Code zu stärken und die Wartbarkeit zu verbessern.

Wie geht das?

Das Schreiben von Tests in Swift ist einfacher als man denkt. Zunächst muss ein neues Test Target erstellt werden, das den Code testen soll. Dann können Tests mit der XCTest-Bibliothek erstellt werden. Hier ist ein einfaches Beispiel für einen Testfall:

```
Swift
class CalculatorTests: XCTestCase {
    func testAddition() {
        let result = Calculator.add(2, 2)
        XCTAssertEqual(result, 4)
    }
}
```

In diesem Beispiel wird überprüft, ob der Additionsfunktion in unserem Calculator-Modul das korrekte Ergebnis zurückgegeben wird. Die `XCTAssertEqual`-Methode vergleicht den tatsächlichen Wert mit dem erwarteten Ergebnis und gibt bei einer Übereinstimmung grün (success) oder bei einer Abweichung rot (failure) aus.

Tiefere Einblicke

Es gibt viele Möglichkeiten, wie Tests in Swift geschrieben werden können. Hier sind einige wichtige Punkte, die berücksichtigt werden sollten:

- Tests sollten so gestaltet sein, dass sie unabhängig voneinander ausgeführt werden können.
- Das Mocken externer Abhängigkeiten kann dabei helfen, die Tests zu isolieren und unabhängig zu machen.
- Es ist wichtig, Edge-Cases in den Tests abzudecken, um sicherzustellen, dass der Code in allen Szenarien korrekt funktioniert.

Siehe auch

- Offizielle Dokumentation zu XCTest: https://developer.apple.com/documentation/xctest
- Ein Leitfaden für das Schreiben von Tests in Swift: https://www.swiftbysundell.com/articles/unit-testing-in-swift/
- Eine Einführung in das Mocking mit Swift: https://medium.com/@joyceekimmocking-in-swift-the-basics-c4b4324bf00a

Mit diesen Informationen sollte es nun möglich sein, qualitativ hochwertige Tests in Swift zu schreiben und so die Qualität und Wartbarkeit des Codes zu verbessern. Viel Spaß beim Testen!