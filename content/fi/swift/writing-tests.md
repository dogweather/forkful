---
title:                "Testien kirjoittaminen"
html_title:           "Swift: Testien kirjoittaminen"
simple_title:         "Testien kirjoittaminen"
programming_language: "Swift"
category:             "Swift"
tag:                  "Testing and Debugging"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fi/swift/writing-tests.md"
---

{{< edit_this_page >}}

## Miksi

Miksi kirjoittaa testejä? Onko se vain turha lisätyö ohjelmointiprosessissa? Ei todellakaan. Hyvin kirjoitetut testit parantavat koodin laatua ja helpottavat ohjelmoijan elämää.

## Miten

Testikoodin kirjoittaminen Swiftissä on helppoa. Perusta testiluokka, jossa testaat haluamiasi toimintoja, ja käytä `XCTAssert`-funktiota arvioidaksesi, onko toiminto toiminut oikein.

```Swift
class CalculatorTests: XCTestCase {
  // Testi lisäysfunktiolle
  func testAddition() {
    let calculator = Calculator()
    let result = calculator.add(a: 2, b: 2)
    XCTAssertEqual(result, 4, "Lisäyksen oletetun tuloksen pitäisi olla 4.")
  }
}
```

Muista myös kirjoittaa positiivisia ja negatiivisia testejä jokaiselle toiminnolle, jotta varmistat koodin toimivuuden kaikissa tilanteissa.

## Syvemmälle

Mikä tekee hyvästä testistä? Ensinnäkin, testin tulee olla yksinkertainen ja selkeä, jotta sen toiminta on helppo ymmärtää. Toiseksi, testin tulee kattaa kaikki tapaukset ja varmistaa, että koodi toimii oikein myös virheellisissä tilanteissa. Lisäksi, testikoodin tulee olla itsenäistä ja toistettavaa, jotta sen avulla voidaan helposti havaita muutokset koodissa.

Muista myös pitää testikoodi ja varsinaisen koodin erillään, jotta muutokset eivät vahingossa vaikuta toisiinsa. Hyvä käytäntö on myös ajaa testit säännöllisesti siirtymällä projektin juureen ja kirjoittamalla `swift test` komento.

## Katso myös

- [XCTest Documentation](https://developer.apple.com/documentation/xctest)
- [Swift Test Driven Development - tutorial](https://www.raywenderlich.com/10730636-test-driven-development-tutorial-for-ios-getting-started)
- [Swift Test Doubles - tutorial](https://www.swiftbysundell.com/articles/mocking-in-swift/)