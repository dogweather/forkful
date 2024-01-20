---
title:                "Skriva tester"
html_title:           "Swift: Skriva tester"
simple_title:         "Skriva tester"
programming_language: "Swift"
category:             "Swift"
tag:                  "Testing and Debugging"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/sv/swift/writing-tests.md"
---

{{< edit_this_page >}}

## Vad & Varför?
I programminuyn så betyder att skriva tester att man skriver kod som kollar att ens program fungerar som det ska. Det kan inkludera att kolla olika användarscenarier och faktiskt köras kod. Programmers skriver tester för att säkerställa att deras kod fungerar korrekt och för att undvika potentiella buggar och fel i framtiden.

## Hur man:
För att skriva tester i Swift, måste du först importera XCTest ramverket. Sedan kan du definiera tester genom att skapa en ny klass som ärver från XCTestCase klassen och använda funktionen XCTFail() för att testa om ett villkor är sant eller falskt. Se exempel nedan:

```Swift 
import XCTest 

class CalculatorTests: XCTestCase {
    func testAdd() {
        let calculator = Calculator()
        let result = calculator.add(5, 2)
        XCTAssertEqual(result, 7)
    }

    func testSubtract() {
        let calculator = Calculator()
        let result = calculator.subtract(5, 2)
        XCTAssertEqual(result, 3)
    }
}

class Calculator {
    func add(_ a: Int, _ b: Int) -> Int {
        return a + b
    }

    func subtract(_ a: Int, _ b: Int) -> Int {
        return a - b
    }
}

```

## Djupdykning:
Tester har funnits sedan programmeringens tidiga dagar. En av de mest populära testningsramverken är JUnit, som är utformat för Java-programmerare. Det finns också alternativ till XCTest för Swift, såsom Quick och Nimble. För att skriva effektiva tester är det viktigt att veta vilka delar av koden som måste testas och att skriva testfall för olika scenarier.

## Se även:
- [Quick: A behavior-driven development framework for Swift](https://github.com/Quick/Quick)
- [Nimble: A matcher framework for Swift and Objective-C](https://github.com/Quick/Nimble)