---
title:                "Skriva tester"
aliases:
- sv/swift/writing-tests.md
date:                  2024-02-03T19:32:04.622849-07:00
model:                 gpt-4-0125-preview
simple_title:         "Skriva tester"
tag:                  "Testing and Debugging"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/sv/swift/writing-tests.md"
changelog:
  - 2024-02-03, gpt-4-0125-preview, translated from English
---

{{< edit_this_page >}}

## Vad & Varför?
Att skriva tester i Swift innebär att skapa och köra kod som verifierar riktigheten hos andra kodenheter i din applikation. Programmerare gör detta för att säkerställa tillförlitlighet, upptäcka buggar tidigt i utvecklingscykeln och underlätta framtida kodrefaktorering utan oavsiktliga konsekvenser.

## Hur:
Swift stöder testning genom sitt XCTest-ramverk, som är integrerat i Xcode. Du kan skriva enhetstester för att verifiera individuella delar av din kod, till exempel en funktion som beräknar summan av två tal.

```swift
import XCTest
@testable import YourApp

class YourAppTests: XCTestCase {

    func testSum() {
        let result = Calculator().sum(a: 1, b: 2)
        XCTAssertEqual(result, 3, "Summafunktionen returnerade inte det förväntade värdet.")
    }
}
```

För att köra detta test skulle du vanligtvis trycka på Command-U i Xcode. Utdata i Xcodes testnavigerare kommer att tala om för dig om testet lyckades eller misslyckades.

Till exempel, en lyckad testutdata:
```
Testfall '-[YourAppTests testSum]' lyckades (0.005 sekunder).
```

För mer avancerade testscenarier, kan du använda tredjepartsbibliotek som Quick/Nimble, vilka erbjuder en mer uttrycksfull syntax för att skriva tester.

Med Quick/Nimble kan du skriva samma test så här:

```swift
// Lägg till Quick och Nimble i din Swift pakethanterare eller använd CocoaPods/Carthage för att installera dem
import Quick
import Nimble
@testable import YourApp

class CalculatorSpec: QuickSpec {
    override func spec() {
        describe("Kalkylatorn") {
            context("när den summerar tal") {
                it("bör den returnera den korrekta summan") {
                    let calculator = Calculator()
                    expect(calculator.sum(a: 1, b: 2)).to(equal(3))
                }
            }
        }
    }
}
```

Att köra detta test skulle ge dig liknande utdata i din testkonsol eller CI/CD-verktygs logg, som anger om testet lyckades eller misslyckades, med ett mer läsbart format för att beskriva tester och förväntningar.
