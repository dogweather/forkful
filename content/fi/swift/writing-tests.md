---
title:                "Swift: Testien kirjoittaminen"
simple_title:         "Testien kirjoittaminen"
programming_language: "Swift"
category:             "Swift"
tag:                  "Testing and Debugging"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fi/swift/writing-tests.md"
---

{{< edit_this_page >}}

## Miksi
Testien kirjoittaminen on tärkeä osa ohjelmointia, sillä se auttaa varmistamaan koodin toimivuuden ja vähentää virheitä. Se myös parantaa koodin luettavuutta ja ylläpidettävyyttä.

## Miten
Seuraavassa esimerkissä näytämme, kuinka voit kirjoittaa yksikkötestin Swiftillä käyttäen XCTest-kirjastoa. Oletetaan, että meillä on funktio, joka laskee kahden luvun summan ja haluamme varmistaa sen toimivuuden.

```Swift
//Luodaan testiluokka
class TestiLuokka: XCTestCase {
	
	//Määritellään testifunktio
	func testiFunktio() {
		
		//Määritellään syötteet
		let luku1 = 5
		let luku2 = 10
		
		//Suoritetaan funktio
		let summa = laskeSumma(luku1: luku1, luku2: luku2)
		
		//Varmistetaan, että funktio palauttaa oikean tuloksen
		XCTAssertEqual(summa, 15)
	}
	
	//Funktio, jonka haluamme testata
	func laskeSumma(luku1: Int, luku2: Int) -> Int {
		return luku1 + luku2
	}
}
```

Kun suoritamme tämän testin, se palauttaa tuloksen "Testi toimi!", mikä tarkoittaa, että testi on onnistunut ja funktio toimii halutusti.

## Syvemmälle
Testien kirjoittamisesta löytyy paljon lisätietoa verkosta, kuten hyödyllisiä vinkkejä ja suosituksia. On myös tärkeää huomata, että testien kirjoittaminen ei ole vain yksikkötestejä, vaan siihen sisältyy myös integraatiotestit ja järjestelmätestit.

## Katso myös
- https://www.raywenderlich.com/709-ios-unit-testing-and-ui-testing-tutorial
- https://developer.apple.com/documentation/xctest