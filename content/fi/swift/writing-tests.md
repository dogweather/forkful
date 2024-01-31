---
title:                "Testien kirjoittaminen"
date:                  2024-01-19
simple_title:         "Testien kirjoittaminen"

tag:                  "Testing and Debugging"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fi/swift/writing-tests.md"
---

{{< edit_this_page >}}

## What & Why?
Testaus tarkoittaa koodin automaattista tarkistamista virheiden varalta. Testit auttavat löytämään ja korjaamaan ongelmia ennen kuin ne päätyvät tuotantoon, minkä ansiosta koodi on luotettavampaa.

## How to:
Swiftissä testejä voidaan kirjoittaa käyttämällä XCTest-kirjaston ominaisuuksia. Tässä esimerkissä luodaan yksinkertainen funktio ja testi sille.

```Swift
// SimpleFunction.swift
func addNumbers(a: Int, b: Int) -> Int {
    return a + b
}

// SimpleFunctionTests.swift
import XCTest
@testable import YourApp

class SimpleFunctionTests: XCTestCase {
    func testAddNumbers() {
        XCTAssertEqual(addNumbers(a: 2, b: 3), 5, "Should add two numbers correctly")
    }
}

// Test output in the console
Test Suite 'All tests' started at 2023-03-10 18:25:54.052
Test Suite 'YourAppTests.xctest' started at 2023-03-10 18:25:54.053
Test Suite 'SimpleFunctionTests' started at 2023-03-10 18:25:54.054
Test Case '-[YourAppTests.SimpleFunctionTests testAddNumbers]' passed (0.001 seconds).
```

## Deep Dive
Swiftin testaustuki on kehittynyt vuosien varrella ja nykyisin XCTest tarjoaa kattavat työkalut eritasoiseen testaukseen. Vaihtoehtoisia testaustyökaluja ovat esimerkiksi Quick ja Nimble, mutta XCTest on Applen virallisesti tukema ja tyypillisesti ensisijainen valinta. Testien kirjoittaminen vaatii ymmärrystä siitä, mitä koodin osia tulee testata ja miten testitulokset interpretoidaan.

## See Also
- Apple's XCTest Documentation: [XCTest](https://developer.apple.com/documentation/xctest)
- Test Driven Development in Swift: [TDD](https://www.raywenderlich.com/21020457-test-driven-development-tutorial-for-ios-getting-started)
- Ray Wenderlich's iOS Unit Testing by Example: [iOS Unit Testing](https://www.raywenderlich.com/960290-ios-unit-testing-and-ui-testing-tutorial)
