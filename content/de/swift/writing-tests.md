---
title:                "Tests schreiben"
date:                  2024-01-19
simple_title:         "Tests schreiben"

tag:                  "Testing and Debugging"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/de/swift/writing-tests.md"
---

{{< edit_this_page >}}

## Was & Warum?
Das Schreiben von Tests überprüft, ob dein Code wie gewünscht funktioniert. Programmierer testen, um Fehler zu minimieren und die Software-Qualität zu sichern.

## Anleitung:
Das Swift-Framework `XCTest` eignet sich zum Schreiben und Ausführen von Tests. Hier ist ein einfacher Test für eine `add` Funktion in Swift:

```Swift
import XCTest

class MathTests: XCTestCase {
    func testAdd() {
        let result = add(2, 3)
        XCTAssertEqual(result, 5, "Das Ergebnis von 2 + 3 sollte 5 sein.")
    }
    
    func add(_ a: Int, _ b: Int) -> Int {
        return a + b
    }
}

MathTests.defaultTestSuite.run()
```

Wenn du das ausführst, kriegst du etwa folgendes als Ausgabe:

```
Test Suite 'MathTests' started at 2023-04-01
Test Case '-[YourProject.MathTests testAdd]' started.
Test Case '-[YourProject.MathTests testAdd]' passed (0.001 seconds).
Test Suite 'MathTests' finished at 2023-04-01.
```

## Tiefere Einblicke:
Tests in Swift gab es schon in Objective-C; da hieß das Framework `SenTestingKit`. `XCTest` ist moderner, schneller, und einfacher zu benutzen. Neben Unit-Tests gibt es auch UI-Tests, bei denen Benutzerinteraktionen simuliert werden. Als Alternative zu `XCTest` kamen Drittanbieter-Tools wie `Quick` und `Nimble` auf, die BDD (Behaviour-Driven Development) unterstützen.

## Weiterführende Infos:
- [Apple XCTest Dokumentation](https://developer.apple.com/documentation/xctest)
- [Artikel zu Behaviour-Driven Development in Swift](https://www.raywenderlich.com/21020457-behavior-driven-development-tutorial-for-ios-with-quick-nimble)
