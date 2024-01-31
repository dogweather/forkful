---
title:                "Pisanie testów"
date:                  2024-01-19
simple_title:         "Pisanie testów"

tag:                  "Testing and Debugging"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pl/swift/writing-tests.md"
---

{{< edit_this_page >}}

## Co i dlaczego?
Pisanie testów to proces tworzenia skryptów, które automatycznie sprawdzają, czy różne części aplikacji działają poprawnie. Robimy to, aby szybko wykrywać błędy, zwiększać pewność kodu i efektywnie wspierać przyszły rozwój projektu.

## Jak to zrobić:
```Swift
import XCTest
@testable import MyAmazingApp

class MyAmazingAppTests: XCTestCase {
    
    func testExample() {
        // Tutaj wpisujemy kod, który chcemy przetestować
        let number = 42
        // Sprawdzamy warunek, czy jest prawdziwy (test powinien się powieść)
        XCTAssertEqual(number, 42, "Number should be equal to 42")
    }
}

// Przykładowe wyjście
// Test Suite 'All tests' started at 2023-03-15 18:23:11.649
// Test Suite 'MyAmazingAppTests' started at 2023-03-15 18:23:11.650
// Test Case '-[MyAmazingAppTests.MyAmazingAppTests testExample]' started.
// Test Case '-[MyAmazingAppTests.MyAmazingAppTests testExample]' passed (0.001 seconds).
```

## Deep Dive
Historia: Pisanie testów jest częścią programowania od początku. Testy manualne były normą, dopóki testy automatyczne nie stały się popularne wraz z rozwojem technologii. Alternatywy: Oprócz XCTest, istnieją inne frameworki, jak Quick/Nimble czy KIF, które służą do testowania aplikacji iOS. Szczegóły implementacyjne: W implementacji testów kluczowe jest ustalanie oczekiwań (`XCTestExpectation`) i asercji, czyli sprawdzeń warunków (`XCTAssert...`).

## Zobacz także
- [Dokumentacja XCTest od Apple](https://developer.apple.com/documentation/xctest)
- [Przewodnik po testach jednostkowych w Swift przez Ray Wenderlich](https://www.raywenderlich.com/960290-ios-unit-testing-and-ui-testing-tutorial)
