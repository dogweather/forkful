---
changelog:
- 2024-02-03, gpt-4-0125-preview, translated from English
date: 2024-02-03 19:32:01.656260-07:00
description: "Kuinka: Swift tukee testausta XCTest-kehikon kautta, joka on integroitu\
  \ Xcodeen. Voit kirjoittaa yksikk\xF6testej\xE4 tarkistamaan koodisi yksitt\xE4\
  isi\xE4 osia,\u2026"
lastmod: '2024-03-13T22:44:56.910413-06:00'
model: gpt-4-0125-preview
summary: Swift tukee testausta XCTest-kehikon kautta, joka on integroitu Xcodeen.
title: Testien kirjoittaminen
weight: 36
---

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
