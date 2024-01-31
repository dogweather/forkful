---
title:                "Writing tests"
date:                  2024-01-19
html_title:           "Arduino recipe: Writing tests"
simple_title:         "Writing tests"

category:             "Swift"
tag:                  "Testing and Debugging"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/en/swift/writing-tests.md"
---

{{< edit_this_page >}}

## What & Why?
Writing tests is crafting code that checks if your software works as planned. Programmers test to catch bugs early, ensure quality, and simplify maintenance.

## How to:
Swift uses XCTest framework for testing. Here's a simple test for a function `add(a:b:)`:

```Swift
import XCTest

class MathTests: XCTestCase {

    func testAdd() {
        let result = add(a: 2, b: 3)
        XCTAssertEqual(result, 5, "Expected 2 + 3 to equal 5")
    }

    func add(a: Int, b: Int) -> Int {
        return a + b
    }
}
```
Run tests with Xcode's Test Navigator or use `cmd+U`. Output should read:

```plaintext
Test Suite 'All tests' passed at ...
    Executed 1 test, with 0 failures (0 unexpected) in 0.001 (0.004) seconds
```

## Deep Dive
XCTest, part of Xcode since 2013, took over from OCUnit. Alternatives are Quick (BDD framework) and SnapshotTesting (UI tests). Testing implementation relies on assertion functions, test cases and optionally test suites, harnessing the XCTest framework's capabilities.

## See Also
- [Apple XCTest Overview](https://developer.apple.com/documentation/xctest)
- [Ray Wenderlich's iOS Unit Testing and UI Testing Tutorial](https://www.raywenderlich.com/21020457-ios-unit-testing-and-ui-testing-tutorial)
- [Testing Swift code with Quick](https://github.com/Quick/Quick)
- [SnapshotTesting on GitHub](https://github.com/pointfreeco/swift-snapshot-testing)
