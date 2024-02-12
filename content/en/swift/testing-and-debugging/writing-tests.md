---
title:                "Writing tests"
aliases:
- /en/swift/writing-tests/
date:                  2024-02-03T19:03:34.797441-07:00
model:                 gpt-4-0125-preview
simple_title:         "Writing tests"
tag:                  "Testing and Debugging"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/en/swift/writing-tests.md"
---

{{< edit_this_page >}}

## What & Why?
Writing tests in Swift involves creating and executing code that verifies the correctness of other code units in your application. Programmers do it to ensure reliability, detect bugs early in the development cycle, and facilitate future code refactoring without unintentional consequences.

## How to:
Swift supports testing through its XCTest framework, which is integrated into Xcode. You can write unit tests to verify individual parts of your code, for example, a function that calculates the sum of two numbers.

```swift
import XCTest
@testable import YourApp

class YourAppTests: XCTestCase {

    func testSum() {
        let result = Calculator().sum(a: 1, b: 2)
        XCTAssertEqual(result, 3, "The sum function did not return the expected value.")
    }
}
```

To run this test, you'd typically press Command-U in Xcode. The output in the Xcode test navigator will tell you if the test passed or failed. 

For example, a successful test output:
```
Test Case '-[YourAppTests testSum]' passed (0.005 seconds).
```

For more advanced testing scenarios, you might adopt third-party libraries such as Quick/Nimble, which offer a more expressive syntax for writing tests.

With Quick/Nimble, you might write the same test like this:

```swift
// Add Quick and Nimble to your Swift package manager or use CocoaPods/Carthage to install them
import Quick
import Nimble
@testable import YourApp

class CalculatorSpec: QuickSpec {
    override func spec() {
        describe("Calculator") {
            context("when summing numbers") {
                it("should return the correct sum") {
                    let calculator = Calculator()
                    expect(calculator.sum(a: 1, b: 2)).to(equal(3))
                }
            }
        }
    }
}
```

Running this test would give you similar output in your test console or CI/CD tool's log, indicating whether the test succeeded or failed, with a more readable format for describing tests and expectations.
