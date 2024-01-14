---
title:    "Swift recipe: Writing tests"
keywords: ["Swift"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/en/swift/writing-tests.md"
---

{{< edit_this_page >}}

## Why

As developers, we all know the importance of writing tests for our code. It helps ensure that our code is functioning correctly and saves us from potential headaches in the future. In this blog post, we'll delve into the world of writing tests in Swift and understand the benefits it brings.

## How To

To write tests in Swift, we'll need to create a separate target in our project with the ".test" extension. Let's name it "MyAppTests". Inside this target, we'll create a new Swift file and name it "MyAppTests.swift".

Now, let's start writing our tests. We'll first need to import XCTest, which is the framework for writing tests in Swift. Then, we can start writing our test cases using the `XCTAssert` assertion functions.

Below is an example of a test case that checks if a given array contains a specific element:

```Swift
import XCTest

class MyAppTests: XCTestCase {
  func testArrayContainsElement() {
    let array = [1, 2, 3, 4]
    XCTAssertTrue(array.contains(3))
  }
}
```

We can also use `XCTAssertEqual` to check for specific values, `XCTAssertNil` to check for nil values, and many more assertion functions provided by XCTest. These tests will run automatically when we build our project, and we'll get a green or red indicator depending on the results.

## Deep Dive

In writing tests, we must make sure that the tests are independent and cover all possible scenarios. We can also use the `setUp` and `tearDown` functions to set up the environment and clean up after each test case, respectively.

In addition, we should also make use of code coverage tools, such as Xcode's built-in coverage profiler, to see which parts of our code are tested and which are not. This helps us identify any missing test cases and ensure that our code is thoroughly tested.

## See Also

- [Official XCTest Framework Documentation](https://developer.apple.com/documentation/xctest)
- [An Introduction to Writing Unit Tests in Swift](https://www.raywenderlich.com/960290-an-introduction-to-writing-unit-tests-in-swift) 
- [Code Coverage Tools for Swift](https://medium.com/@jamesrochabrun/ios-code-coverage-tools-259c74dd01da)