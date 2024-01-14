---
title:                "Swift: Testen schreiben"
programming_language: "Swift"
category:             "Testing and Debugging"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/de/swift/writing-tests.md"
---

{{< edit_this_page >}}

## Warum

Warum sollte man Tests in Swift schreiben? Tests garantieren eine höhere Codequalität und sorgen dafür, dass der Code zuverlässig funktioniert. Sie helfen auch dabei, Fehler frühzeitig zu erkennen und zu beheben, bevor sie zu großen Problemen werden.

## Wie man Tests in Swift schreibt

Das Schreiben von Tests in Swift ist einfach und kann mit nur wenigen Zeilen Code erledigt werden. Hier ist ein Beispiel, wie man einen einfachen Testfall schreibt:

```Swift
func testAddition() {
  let calculator = Calculator()
  let result = calculator.add(2, 3)
  XCTAssertEqual(result, 5)
}
```

In diesem Beispiel wird die Funktion `testAddition` definiert, die einen Taschenrechner erstellt und die `add`-Funktion mit zwei Zahlen aufruft. Dann wird überprüft, ob das Ergebnis der Addition dem erwarteten Wert entspricht. Wenn ja, wird der Test bestanden.

## Tiefergehende Informationen zum Schreiben von Tests

Beim Schreiben von Tests gibt es einige wichtige Dinge zu beachten. Zum Beispiel sollte man sicherstellen, dass die Testfälle unabhängig voneinander sind und es keine Abhängigkeiten zwischen ihnen gibt. Außerdem sollte man sicherstellen, dass alle möglichen Fälle abgedeckt sind und auch Randfälle getestet werden. Eine gute Testabdeckung stellt sicher, dass der Code zuverlässig funktioniert.

Es gibt auch verschiedene Arten von Tests, wie zum Beispiel Unit-Tests, Integrationstests und UI-Tests. Jede Art hat ihre eigene Funktion und sollte je nach Bedarf verwendet werden.

## Siehe auch

- [Apple Dokumentation zu Unit-Tests](https://developer.apple.com/documentation/xctest)

- [Tutorial zum Schreiben von Tests in Swift](https://www.raywenderlich.com/960290-ios-unit-testing-and-ui-testing-tutorial)

- [Testing Guidelines von Apple](https://developer.apple.com/library/archive/documentation/DeveloperTools/Conceptual/testing_with_xcode/chapters/04-writing_tests.html)