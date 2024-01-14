---
title:    "Swift: Testausten kirjoittaminen"
keywords: ["Swift"]
---

{{< edit_this_page >}}

## Miksi

Ohjelmistokehittäjät usein jättävät testien kirjoittamisen vähemmälle huomiolle, mutta se on yksi tärkeimmistä osa-alueista ohjelmiston laadun varmistamisessa. Testauksen avulla voit havaita mahdollisia virheitä ja bugeja koodissasi ennen niiden saamista käyttäjien käsiin. 

## Miten

Testien kirjoittaminen Swift-ohjelmassa on helppoa ja vaivatonta. Voit aloittaa lisäämällä `XCTest` -kirjaston projektiisi. Kirjoita sitten testifunktio, joka tarkistaa haluamasi ominaisuuden toimivuuden. Alla on esimerkki testifunktiosta, joka tarkistaa, onko annettu luku parillinen vai ei.

```Swift
func testEvenNumber() {
    let number = 4
    XCTAssertTrue(number % 2 == 0, "The number is not even")
}
```

Kun suoritat nämä testit, saat joko virheen, jos testi ei onnistu, tai tiedon siitä, että testi suoritettiin onnistuneesti.

```
Test Suite 'All tests' started at 2021-04-05 14:30:25.043
Test Suite 'TestExampleTests.xctest' started at 2021-04-05 14:30:25.043
Test Suite 'TestExampleTests' started at 2021-04-05 14:30:25.043
Test Case '-[TestExampleTests.TestExampleTests testEvenNumber]' started.
Test Case '-[TestExampleTests.TestExampleTests testEvenNumber]' passed (0.001 seconds).
Test Suite 'TestExampleTests' passed at 2021-04-05 14:30:25.044.
	 Executed 1 test, with 0 failures (0 unexpected) in 0.001 (0.001) seconds
Test Suite 'TestExampleTests.xctest' passed at 2021-04-05 14:30:25.045.
	 Executed 1 test, with 0 failures (0 unexpected) in 0.001 (0.002) seconds
Test Suite 'All tests' passed at 2021-04-05 14:30:25.045.
	 Executed 1 test, with 0 failures (0 unexpected) in 0.001 (0.004) seconds
```

## Syventävä sukellus

Testien kirjoittaminen voi tuntua aikaa vievältä, mutta se auttaa sinua pitkällä aikavälillä. Kun uutta koodia lisätään tai vanhaa muokataan, voit nopeasti suorittaa testit ja varmistua siitä, että koodisi edelleen toimii halutulla tavalla. Voit myös helposti löytää virheitä ja bugeja, jos testit epäonnistuvat.

On myös tärkeää muistaa, että testien tulee olla riippumattomia ja erillisiä toisistaan. Yhden testin epäonnistuminen ei saisi vaikuttaa muihin testeihin, jotta varmistetaan luotettavat tulokset.

## Katso myös

- [Swift Test Driven Development Tutorial](https://www.raywenderlich.com/960290-ios-unit-testing-and-ui-testing-tutorial)
- [Basics of Test Driven Development in Swift 5](https://medium.com/sis-software-engineering/basics-of-test-driven-development-in-swift-5-d8d876b19db6)
- [Getting Started with XCTest Framework in Swift](https://learnappmaking.com/xctest-testing-swift-how-to/)