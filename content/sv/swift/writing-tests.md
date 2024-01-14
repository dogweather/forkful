---
title:    "Swift: Skriva tester"
keywords: ["Swift"]
---

{{< edit_this_page >}}

## Varför

Att skriva tester är en viktig del av Swift-programmering eftersom det hjälper till att säkerställa att koden fungerar som den ska och minimerar risken för buggar. Det kan också leda till en snabbare utvecklingsprocess och ökad kvalitet på koden.

## Hur man gör det

En av de vanligaste sätten att skriva tester i Swift är att använda sig av XCTest-ramverket. Detta är ett inbyggt ramverk i Swift som gör det enkelt att skriva, köra och debugga tester.

För att använda XCTest behöver du en testklass, som är en Swift-fil som slutar på *Tests*. I denna fil kan du sedan skriva dina tester genom att använda olika metoder som *XCTAssertEquals* eller *XCTAssertTrue*. Dessa metoder tar in två värden och jämför dem för att se om de är lika och returnerar ett godkännande eller ett felmeddelande beroende på resultatet.

Här är ett exempel på hur en testklass kan se ut i Swift:

```Swift
import XCTest

class PersonTests: XCTestCase {

    func testPersonName() {
        // Skapar ett nytt person-objekt
        let person = Person(name: "Lisa")

        // Jämför det förväntade namnet med det faktiska namnet på personen
        XCTAssertEqual(person.name, "Lisa")
    }
}

```

När du har skrivit dina tester kan du köra dem genom att trycka på den mjölkvita triangelsymbolen bredvid dina testmetoder. Om alla tester passerar kommer du få ett grönt godkännande, annars kommer du få ett rött felmeddelande som indikerar vilket test som har misslyckats.

## Djupdykning

Förutom de grundläggande metoderna som nämndes i föregående avsnitt, finns det många andra funktioner och metoder som du kan använda för att skriva mer avancerade tester. Till exempel kan du använda *XCTAssertThrowsError* för att testa att en viss kod faktiskt genererar ett felmeddelande, eller *XCTKVOExpectation* för att testa asynkron kod.

Det finns också olika tekniker för att organisera dina tester, till exempel genom att använda *setUp()* och *tearDown()* metoder för att utföra kod som behövs innan och efter varje test.

Det är också viktigt att komma ihåg att skriva bra tester handlar inte bara om att ha en hög täckningsgrad, utan också om att testa olika fall och scenarier för att säkerställa att din kod fungerar som den ska i olika situationer.

## Se även

Här är några länkar med mer information om att skriva tester i Swift:

- [Swift.org - Gra funktionsprovning i Swift](https://swift.org/blog/unit-testing/) 
- [SwiftyJSON - Testing Swift Package](https://github.com/SwiftyJSON/SwiftyJSON#testing-swift-package)
- [AppCoda - Getting Started with Swift Unit Testing in Xcode](https://www.appcoda.com/unit-testing-swift/)