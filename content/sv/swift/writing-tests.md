---
title:                "Skriva tester"
html_title:           "Arduino: Skriva tester"
simple_title:         "Skriva tester"
programming_language: "Swift"
category:             "Swift"
tag:                  "Testing and Debugging"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/sv/swift/writing-tests.md"
---

{{< edit_this_page >}}

## Vad & Varför?
Att skriva tester innebär att koda speciella funktioner som kontrollerar att din kod gör det den ska. Programmerare gör detta för att hitta buggar tidigt, säkra kodkvalitet och förenkla framtida underhåll.

## Gör så här:
Swift använder XCTest-ramverket för att hantera testning. Här är en enkel XCTest-fall för att testa en funktion som adderar två tal:

```Swift
import XCTest

// Din funktion som ska testas
func addera(_ a: Int, _ b: Int) -> Int {
    return a + b
}

// Testfall
class MathTests: XCTestCase {
    func testAddera() {
        XCTAssertEqual(addera(2, 3), 5, "Adderingsfunktionen borde ge 5 när vi adderar 2 och 3")
    }
}

// TestRunner (normalt automatiskt hanterad av Xcode)
XCTMain([
    testCase(MathTests.allTests)
])
```

Resultat:
```
Test Suite 'All tests' started at 2023-03-29 15:23:17.170
Test Suite 'MathTests' started at 2023-03-29 15:23:17.172
Test Case '-[MathTests testAddera]' started.
Test Case '-[MathTests testAddera]' passed (0.001 seconds).
Test Suite 'MathTests' finished at 2023-03-29 15:23:17.173.
```

## Deep Dive
Tidiga versioner av Swift hade inget inbyggt stöd för tester, men XCTest, som härstammar från OCUnit för Objective-C, infördes i Swift för att möjliggöra TDD (Test-Driven Development). Alternativ till XCTest inkluderar Quick/Nimble som erbjuder en mer beskrivande BDD-stil (Behavior-Driven Development). När du skriver tester är det viktigt att hålla dem isolerade och snabbkörande för att effektivisera utvecklingscykeln.

## Se även:
- [Apple's dokumentation av XCTest](https://developer.apple.com/documentation/xctest)
- [Artikel om Test-Driven Development i Swift](https://www.raywenderlich.com/5522-test-driven-development-tutorial-for-ios-getting-started)
- [Quick GitHub repository](https://github.com/Quick/Quick)

**Observera:** Länkarna är på engelska då robusta Swift-resurser på svenska är begränsade.
