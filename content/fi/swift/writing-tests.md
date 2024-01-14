---
title:    "Swift: Testien kirjoittaminen"
keywords: ["Swift"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/fi/swift/writing-tests.md"
---

{{< edit_this_page >}}

## Miksi

Testien kirjoittaminen voi vaikuttaa ylimääräiseltä työltä, mutta se on erittäin tärkeää ohjelmointiprosessin kannalta. Se auttaa varmistamaan koodin toimivuuden ja vähentää mahdollisia virheitä ja bugeja myöhemmässä vaiheessa. Lisäksi se helpottaa myös koodin ymmärtämistä ja kehittämistä.

## Kuinka

Testien kirjoittaminen Swiftillä on yksinkertaista ja seuraavassa on muutamia esimerkkejä siitä, miten voit tehdä sen:

```Swift
func sumNumbers(num1: Int, num2: Int) -> Int {
  return num1 + num2
}

// Testataan funktiota sumNumbers
assert(sumNumbers(num1: 2, num2: 3) == 5)
assert(sumNumbers(num1: 6, num2: -3) == 3)
```

Nämä testit varmistavat, että funktio sumNumbers toimii oikein antamillamme parametreilla. Voit myös käyttää XCTestin testiluokkia ja -metodeja testien kirjoittamiseen.

```Swift
// Luodaan testiluokka
class MathTests: XCTestCase {
  var math: Math!
  
  // Alustetaan math-instanssi ennen jokaista testiä
  override func setUp() {
    super.setUp()
    math = Math()
  }
  
  // Testataan sumNumbers-metodia
  func testSumNumbers() {
    XCTAssertEqual(math.sumNumbers(num1: 2, num2: 3), 5)
    XCTAssertEqual(math.sumNumbers(num1: 6, num2: -3), 3)
  }
}

// Ajetaan testit
XCTMain([testCase(MathTests.allTests)])
```

Nämä ovat vain muutamia esimerkkejä testien kirjoittamisesta Swiftillä. Voit löytää lisätietoja ja -ohjeita Apple Developer Documentationista.

## Syventyvä kurkistus

Testien kirjoittaminen voi vaikuttaa tylsältä ja aikaa vievältä prosessilta, mutta se säästää aikaa ja vaivaa myöhemmin. Kun testit on kirjoitettu kattavasti, koodia on helpompi ylläpitää ja kehittää. Lisäksi se auttaa myös uusien kehittäjien pääsemistä nopeasti sisään projektiin ja ymmärtämään koodin toimintaa.

Yksi tärkeä huomio on myös testien ajankohtaisuus. On tärkeää varmistaa, että testit ovat ajan tasalla ja toimivat koodin muutosten jälkeen. Muuten ne menettävät merkityksensä ja voivat johtaa virheisiin.

## Katso myös

- Apple Developer Documentation: https://developer.apple.com/documentation/xctest
- Swift Test-Driven Development by Jon Reid: https://github.com/jonreid/SwiftTDDWorkshop
- Kaleidosoft Blog: https://kaleidosoftlabs.com/blog/tdd-swift/