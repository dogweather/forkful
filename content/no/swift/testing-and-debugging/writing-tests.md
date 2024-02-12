---
title:                "Skrive tester"
aliases:
- /no/swift/writing-tests/
date:                  2024-02-03T19:32:01.333725-07:00
model:                 gpt-4-0125-preview
simple_title:         "Skrive tester"
tag:                  "Testing and Debugging"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/no/swift/writing-tests.md"
changelog:
  - 2024-02-03, gpt-4-0125-preview, translated from English
---

{{< edit_this_page >}}

## Hva & Hvorfor?
Å skrive tester i Swift innebærer å lage og utføre kode som verifiserer korrektheten av andre kodeenheter i applikasjonen din. Programmerere gjør dette for å sikre pålitelighet, oppdage feil tidlig i utviklingssyklusen og lette fremtidig kodeomstrukturering uten utilsiktede konsekvenser.

## Hvordan:
Swift støtter testing gjennom sitt XCTest-rammeverk, som er integrert i Xcode. Du kan skrive enhetstester for å verifisere individuelle deler av koden din, for eksempel en funksjon som beregner summen av to tall.

```swift
import XCTest
@testable import YourApp

class YourAppTests: XCTestCase {

    func testSum() {
        let result = Calculator().sum(a: 1, b: 2)
        XCTAssertEqual(result, 3, "Summefunksjonen returnerte ikke den forventede verdien.")
    }
}
```

For å kjøre denne testen vil du vanligvis trykke Command-U i Xcode. Utdataen i Xcode-testnavigatoren vil fortelle deg om testen besto eller feilet.

For eksempel en vellykket testutdata:
```
Test Case '-[YourAppTests testSum]' passed (0.005 seconds).
```

For mer avanserte testsenarioer kan du ta i bruk tredjeparts biblioteker som Quick/Nimble, som tilbyr en mer uttrykksfull syntaks for å skrive tester.

Med Quick/Nimble kan du skrive samme test slik:

```swift
// Legg til Quick og Nimble i din Swift pakkebehandler eller bruk CocoaPods/Carthage for å installere dem
import Quick
import Nimble
@testable import YourApp

class CalculatorSpec: QuickSpec {
    override func spec() {
        describe("Calculator") {
            context("når man summerer tall") {
                it("skal returnere den korrekte summen") {
                    let calculator = Calculator()
                    expect(calculator.sum(a: 1, b: 2)).to(equal(3))
                }
            }
        }
    }
}
```

Å kjøre denne testen ville gitt deg lignende utdata i din testkonsoll eller CI/CD-verktøyets logg, som indikerer om testen lyktes eller feilet, med et mer leselig format for å beskrive tester og forventninger.
