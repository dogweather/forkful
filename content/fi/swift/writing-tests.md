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

## Mikä & Miksi?

Testien kirjoittaminen on ohjelmistokehittäjien tapa varmistaa, että koodi toimii oikein ja pysyy virheettömänä. Tämä auttaa vähentämään virheitä ja vikatilanteita ohjelmistossa, mikä johtaa parempaan lopputuotteeseen ja tyytyväisempiin käyttäjiin.

## Miten:

```Swift
func sumOfTwoNumbers(_ num1: Int, _ num2: Int) -> Int {
    return num1 + num2
}

test("Sum of 2 and 3 is 5") {
    expect(sumOfTwoNumbers(2, 3)).toBe(5)
}
```

Testikoodin kirjoittaminen on yksinkertaista. Ensiksi määrität funktion tai metodin, jolle haluat kirjoittaa testin. Sitten käytät testikehystä, kuten esimerkissä käytettyä `test`-funktiota, joka ottaa parametreina nimen ja testikoodin. Lopuksi käytät `expect`-funktiota, joka verrataan testin tulokseen ja odotettuun arvoon.

## Syväsukellus:

Testien kirjoittaminen on ollut osa ohjelmistokehitystä jo vuosikymmenien ajan. Perinteisesti se on tehty manuaalisesti, mutta nykyään on olemassa myös automatisoituja testikehyksiä, kuten esimerkiksi Swiftin oma XCTest. Näiden avulla testien kirjoittaminen on nopeampaa ja luotettavampaa.

On myös olemassa erilaisia testaustapoja, kuten yksikkötestaus, integraatiotestaus ja hyväksymistestaus. Jokaisella on oma tarkoituksensa ja ne kaikki ovat tärkeitä osia ohjelmiston laadun varmistamisessa.

## Tsekkaa myös:

- [XCTest Documentation](https://developer.apple.com/documentation/xctest)
- [Unit Testing in Swift](https://www.raywenderlich.com/960290-ui-testing-and-continuous-integration-with-xcode-and-swift)