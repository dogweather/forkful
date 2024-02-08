---
title:                "Scrivere test"
aliases:
- it/swift/writing-tests.md
date:                  2024-02-03T19:32:02.171963-07:00
model:                 gpt-4-0125-preview
simple_title:         "Scrivere test"
tag:                  "Testing and Debugging"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/it/swift/writing-tests.md"
changelog:
  - 2024-02-03, gpt-4-0125-preview, translated from English
---

{{< edit_this_page >}}

## Cosa e Perché?
Scrivere test in Swift comporta la creazione e l'esecuzione di codice che verifica la correttezza di altre unità di codice nella tua applicazione. I programmatori lo fanno per garantire affidabilità, rilevare bug all'inizio del ciclo di sviluppo e facilitare il futuro refactoring del codice senza conseguenze non intenzionali.

## Come fare:
Swift supporta i test attraverso il suo framework XCTest, che è integrato in Xcode. Puoi scrivere test unitari per verificare singole parti del tuo codice, per esempio, una funzione che calcola la somma di due numeri.

```swift
import XCTest
@testable import TuaApp

class TestTuaApp: XCTestCase {

    func testaSomma() {
        let risultato = Calcolatrice().somma(a: 1, b: 2)
        XCTAssertEqual(risultato, 3, "La funzione somma non ha restituito il valore atteso.")
    }
}
```

Per eseguire questo test, in genere premi Command-U in Xcode. L'output nel navigatore di test di Xcode ti dirà se il test è passato o fallito.

Per esempio, un output di test riuscito:
```
Caso di Test '-[TestTuaApp testaSomma]' superato (0.005 secondi).
```

Per scenari di test più avanzati, potresti adottare librerie di terze parti come Quick/Nimble, che offrono una sintassi più espressiva per scrivere test.

Con Quick/Nimble, potresti scrivere lo stesso test così:

```swift
// Aggiungi Quick e Nimble al tuo gestore di pacchetti Swift o usa CocoaPods/Carthage per installarli
import Quick
import Nimble
@testable import TuaApp

class SpecificaCalcolatrice: QuickSpec {
    override func spec() {
        describe("Calcolatrice") {
            context("quando somma numeri") {
                it("dovrebbe restituire la somma corretta") {
                    let calcolatrice = Calcolatrice()
                    expect(calcolatrice.somma(a: 1, b: 2)).to(equal(3))
                }
            }
        }
    }
}
```

L'esecuzione di questo test ti darebbe un output simile nel tuo console di test o nel log dello strumento CI/CD, indicando se il test è riuscito o fallito, con un formato più leggibile per descrivere test ed aspettative.
