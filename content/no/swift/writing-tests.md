---
title:                "Skriving av tester"
html_title:           "Arduino: Skriving av tester"
simple_title:         "Skriving av tester"
programming_language: "Swift"
category:             "Swift"
tag:                  "Testing and Debugging"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/no/swift/writing-tests.md"
---

{{< edit_this_page >}}

## What & Why?
Tester er rutiner som sjekker om kode fungerer som forventet. De reduserer feil og forbedrer kodekvalitet ved å sikre at endringer ikke knekker funksjonalitet.

## How to:
Swift bruker XCTest-rammeverket for testing. Her er et enkelt eksempel som tester en funksjon `leggTil(a:b:)`.

```Swift
import XCTest

class MinMatteTests: XCTestCase {
    
    func testLeggTil() {
        let resultat = leggTil(a: 2, b: 3)
        XCTAssertEqual(resultat, 5, "Feil: Forventet 5, fikk \(resultat)")
    }

    func leggTil(a: Int, b: Int) -> Int {
        return a + b
    }
}

// Sample Output:
// Test Case '-[MinMatteTests testLeggTil]' passed (0.001 seconds).
```

## Deep Dive
Før XCTest kom Objective-C sin SenTestingKit. Alternativer inkluderer Quick og Nimble som gir mer beskrivende syntaks. Effektiv testing krever forståelse av begreper som `mocks`, `stubs` og `fake` objekter for å isolere testkoden.

## See Also
- [Apple Developer Documentation](https://developer.apple.com/documentation/xctest)
- [Ray Wenderlich - iOS Unit Testing and UI Testing Tutorial](https://www.raywenderlich.com/21020457-ios-unit-testing-and-ui-testing-tutorial)
- [Quick GitHub](https://github.com/Quick/Quick)
- [Nimble GitHub](https://github.com/Quick/Nimble)
