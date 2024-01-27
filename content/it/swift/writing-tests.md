---
title:                "Scrivere test"
date:                  2024-01-19
html_title:           "Arduino: Scrivere test"
simple_title:         "Scrivere test"
programming_language: "Swift"
category:             "Swift"
tag:                  "Testing and Debugging"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/it/swift/writing-tests.md"
---

{{< edit_this_page >}}

## What & Why?
Scrivere test è l'atto di creare mini programmi che controllano se il codice principale funziona correttamente. I programmatori li usano per assicurarsi che il codice faccia esattamente quello che si aspettano, riducendo errori e bug.

## How to:
Installa XCTest e crea un caso di test. Ecco un esempio semplice:

```Swift
import XCTest

// Codice da testare
func somma(a: Int, b: Int) -> Int {
    return a + b
}

// Caso di test
class TestMatematico: XCTestCase {
    func testSomma() {
        XCTAssertEqual(somma(a: 2, b: 3), 5, "La somma di 2 e 3 dovrebbe essere 5")
    }
}

// Esegui i test
TestMatematico.defaultTestSuite.run()
```

Output di esempio:

```
Test Suite 'TestMatematico' started at 2023-04-01 18:45:23.123
Test Case '-[TestMatematico testSomma]' started.
Test Case '-[TestMatematico testSomma]' passed (0.001 seconds).
Test Suite 'TestMatematico' finished at 2023-04-01 18:45:23.124.
```

## Deep Dive:
XCTest è il framework usato in Swift per scrivere test unitari ed è integrato in Xcode dal 2013. Prima si usava OCUnit, ma XCTest è diventato standard per la compatibilità e facilità d'uso. Oltre ai test unitari, ci sono anche i test di UI con XCUITest e il test-driven development (TDD) dove si scrivono i test prima del codice.

## See Also:
- [Testing with Xcode](https://developer.apple.com/documentation/xctest)
- [Ray Wenderlich - Unit Testing Tutorial](https://www.raywenderlich.com/960290-ios-unit-testing-and-ui-testing-tutorial)
