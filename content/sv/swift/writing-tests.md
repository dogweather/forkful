---
title:                "Swift: Skriva tester"
programming_language: "Swift"
category:             "Testing and Debugging"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/sv/swift/writing-tests.md"
---

{{< edit_this_page >}}

## Varför
Att skriva tester är en viktig del av att utveckla pålitliga program. Tester hjälper till att upptäcka och förebygga buggar och felaktigheter i koden innan den når slutanvändare.

## Så här gör du
För att skriva tester i Swift behöver du först importera XCTest-frameworket. Sedan kan du skapa en testklass och definiera testfall med hjälp av metoden `func test()`. Inuti varje testfall kan du använda olika assertions för att verifiera att den testade koden uppför sig som förväntat. Här är ett enkelt exempel på en testklass som kontrollerar att en funktion returnerar det förväntade resultatet:

```Swift
import XCTest

class MyTests: XCTestCase {
    func testAddition() {
        let result = addNumbers(a: 2, b: 5)
        XCTAssertEqual(result, 7)
    }
}

func addNumbers(a: Int, b: Int) -> Int {
    return a + b
}

MyTests.defaultTestSuite.run()
```

I det här exemplet importerar vi XCTest och skapar sedan en klass för våra tester. I testfallet `testAddition` anropar vi en funktion för att lägga ihop två tal och använder sedan `XCTAssertEqual` för att verifiera att det returnerade resultatet är 7. Slutligen kör vi alla testfall med `defaultTestSuite.run()`.

Det finns många olika assertions som du kan använda beroende på vad du behöver testa. Du kan läsa mer om dem i dokumentationen för XCTest.

## Djupdykning
Det finns flera anledningar till varför det är viktigt att skriva tester för din kod. Förutom att upptäcka buggar tidigt och förebygga dem, kan tester också fungera som en form av dokumentation för din kod. Genom att skriva tester tvingas du också att tänka på olika scenarier och kan identifiera användningsfall som du kanske inte hade tänkt på annars.

Att skriva tester hjälper också till att hålla din kod baserad på best practices och god kodstil. Ju mer du använder tester i din utvecklingsprocess, desto lättare blir det att upprätthålla kvaliteten på din kodbas.

## Se även
- [Apple Dokumentation - XCTest](https://developer.apple.com/documentation/xctest)
- [Ray Wenderlich Tutorial on Unit Testing in Swift](https://www.raywenderlich.com/709-ios-unit-testing-and-ui-testing-tutorial)
- [Swift by Sundell - Unit Testing](https://www.swiftbysundell.com/basics/unit-testing/)