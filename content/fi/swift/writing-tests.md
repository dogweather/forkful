---
title:                "Testien kirjoittaminen"
aliases:
- fi/swift/writing-tests.md
date:                  2024-02-03T19:32:01.656260-07:00
model:                 gpt-4-0125-preview
simple_title:         "Testien kirjoittaminen"
tag:                  "Testing and Debugging"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fi/swift/writing-tests.md"
changelog:
  - 2024-02-03, gpt-4-0125-preview, translated from English
---

{{< edit_this_page >}}

## Mikä & Miksi?
Testien kirjoittaminen Swiftillä sisältää koodin luomista ja suorittamista, joka varmistaa muiden sovelluksesi koodiyksiköiden oikeellisuuden. Ohjelmoijat tekevät sen varmistaakseen luotettavuuden, havaitakseen virheet varhaisessa kehitysvaiheessa ja helpottaakseen tulevia koodin uudelleenjärjestelyjä ilman tahattomia seurauksia.

## Kuinka:
Swift tukee testausta XCTest-kehikon kautta, joka on integroitu Xcodeen. Voit kirjoittaa yksikkötestejä tarkistamaan koodisi yksittäisiä osia, esimerkiksi funktion, joka laskee kahden luvun summan.

```swift
import XCTest
@testable import YourApp

class YourAppTests: XCTestCase {

    func testSum() {
        let result = Calculator().sum(a: 1, b: 2)
        XCTAssertEqual(result, 3, "Summa-funktio ei palauttanut odotettua arvoa.")
    }
}
```

Tämän testin suorittamiseen painaisit tyypillisesti Command-U:tä Xcodessa. Xcoden testinavigaattorin tulos kertoo, menikö testi läpi vai ei.

Esimerkiksi onnistuneen testin tulos:
```
Testitapaus '-[YourAppTests testSum]' meni läpi (0.005 sekuntia).
```

Edistyneemmissä testausskenaarioissa saatat ottaa käyttöön kolmannen osapuolen kirjastoja, kuten Quick/Nimble, jotka tarjoavat ilmaisuvoimaisempaa syntaksia testien kirjoittamiseen.

Quick/Nimblen avulla saatat kirjoittaa saman testin näin:

```swift
// Lisää Quick ja Nimble Swift-paketinhallintaasi tai käytä CocoaPodseja/Carthagea asentaaksesi ne
import Quick
import Nimble
@testable import YourApp

class CalculatorSpec: QuickSpec {
    override func spec() {
        describe("Laskin") {
            context("kun lasketaan numeroiden summaa") {
                it("pitäisi palauttaa oikea summa") {
                    let laskin = Calculator()
                    expect(laskin.sum(a: 1, b: 2)).to(equal(3))
                }
            }
        }
    }
}
```

Tämän testin suorittaminen antaisi sinulle samankaltaisen tuloksen testikonsolissasi tai CI/CD-työkalusi lokissa, ilmoittaen, menikö testi läpi vai ei, luettavammassa muodossa testien ja odotusten kuvaamiseksi.
