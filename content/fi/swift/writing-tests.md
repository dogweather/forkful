---
title:                "Swift: Testien kirjoittaminen"
programming_language: "Swift"
category:             "Testing and Debugging"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fi/swift/writing-tests.md"
---

{{< edit_this_page >}}

## Miksi

Kirjoitettaessa koodia, testien lisääminen on usein tehtävä, joka jätetään viimeiseksi. Mutta testien kirjoittaminen jo koodin luomisen alkuvaiheessa voi säästää paljon aikaa ja vaivaa myöhemmin. Testit auttavat varmistamaan, että koodi toimii oikein ja estävät mahdollisia virheitä. Ne myös helpottavat koodin hahmottamista ja ylläpitämistä. Lue lisää testien kirjoittamisesta tässä blogissa.

## Miten

Testien kirjoittamisen aloittaminen Swiftissä on helppoa. Tässä on esimerkki, kuinka voit luoda yksinkertaisen testin funkcionaliteetille:

```Swift
func laskeSumma(_ numero1: Int, _ numero2: Int) -> Int {
    return numero1 + numero2
}

let tulos = laskeSumma(5, 3)
print(tulos)
```

Koodi tulostaa luvun 8, mikä on odotettu tulos. Nyt voimme luoda testin, joka vahvistaa tämän tuloksen:

```Swift
func testLaskeSumma() {
    let tulos = laskeSumma(5, 3)
    assert(tulos == 8, "Tuloksen pitäisi olla 8.")
}

testLaskeSumma()
```

Kun suoritat tämän testin, sinun pitäisi nähdä konsolissa ilmoitus "Testi suoritettu onnistuneesti." Tämä tarkoittaa, että koodimuutoksesi eivät ole vaikuttaneet haluttuun lopputulokseen ja että testi toimii oikein.

## Syväsukellus

Testien kirjoittaminen Swiftissä on tärkeä osa koodin laadun varmistamista. Ne auttavat estämään virheitä ja tekevät koodin ylläpidosta sujuvampaa. Testien avulla voit myös testata monimutkaisia toimintoja ja varmistaa, että kaikki reunatapaukset on otettu huomioon.

Testien kirjoittamisen lisäksi on tärkeää myös varmistaa, että koodi on helposti testattavissa ja että sinulla on tehokas testausjärjestelmä. Swiftissä on käytettävissä useita testaustyökaluja, kuten Xcode ja XCTest. Nämä työkalut tarjoavat helpotusta testien kirjoittamiseen ja suorittamiseen.

## Katso myös

- [Swiftin testausdokumentaatio] (https://developer.apple.com/documentation/xctest/testing_swift_code)
- [Hyviä käytäntöjä Swift-testien kirjoittamiseen] (https://www.swiftbysundell.com/articles/unit-testing-in-swift/)
- [Testien priorisointi Swiftissä] (https://www.vadimbulavin.com/swift-unit-testing-tips-and-tricks/)

---

## Katso myös